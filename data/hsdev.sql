-- system table for various options, for now only version
create table hsdev (
	option text,
	value json
);

-- packages per package-db
create table package_dbs (
	package_db text not null, -- global, user or path
	package_name text not null,
	package_version text not null
);

create index package_names_index on package_dbs (package_db, package_name);

-- same as `package_dbs`, but only latest version of package left
create view latest_packages (
	package_db,
	package_name,
	package_version
) as
select package_db, package_name, max(package_version)
from package_dbs
group by package_db, package_name;

-- sandboxes
create table sandboxes (
	type text not null, -- cabal/stack
	path text not null, -- sandbox path, should include `.cabal-sandbox`/`.stack-work`
	package_db_stack json -- list of package-db of sandbox
);

create unique index sandboxes_path_index on sandboxes (path);

-- source projects
create table projects (
	id integer primary key autoincrement,
	name text not null,
	cabal text not null, -- path to `.cabal` file
	version text,
	build_tool text not null, -- cabal/stack
	package_db_stack json -- list of package-db
);

create unique index projects_id_index on projects (id);
create unique index projects_cabal_index on projects (cabal);

-- project's library targets
create table libraries (
	project_id integer not null,
	modules json not null, -- list of modules
	build_info_id integer not null
);

create unique index libraries_ids_index on libraries (project_id, build_info_id);

-- project's executables targets
create table executables (
	project_id integer not null,
	name text not null,
	path text not null,
	build_info_id integer not null
);

create unique index executables_ids_index on executables (project_id, build_info_id);

-- project's tests targets
create table tests (
	project_id integer not null,
	name text not null,
	enabled integer not null,
	main text,
	build_info_id integer not null
);

create unique index tests_ids_index on tests (project_id, build_info_id);

-- map from project to build-info for all targets
create view targets (
	project_id,
	build_info_id
) as
select project_id, build_info_id from libraries
union
select project_id, build_info_id from executables
union
select project_id, build_info_id from tests;

-- target build-info
create table build_infos(
	id integer primary key autoincrement,
	depends json not null, -- list of dependencies
	language text,
	extensions json not null, -- list of extensions
	ghc_options json not null, -- list of ghc-options
	source_dirs json not null, -- list of source directories
	other_modules json not null -- list of other modules
);

create unique index build_infos_id_index on build_infos (id);

-- project dependent packages
create view projects_deps (
	cabal,
	package_db,
	package_name,
	package_version
) as
select distinct p.cabal, ps.package_db, ps.package_name, ps.package_version
from projects as p, json_each(p.package_db_stack) as pdb_stack, build_infos as b, json_each(b.depends) as deps, targets as t, latest_packages as ps
where
	(p.id == t.project_id) and
	(b.id == t.build_info_id) and
	(deps.value <> p.name) and
	(ps.package_name == deps.value) and
	(ps.package_db == pdb_stack.value);

-- packages in scope of project
-- `cabal` may be null for standalone files, in this case all 'user-db' is in scope
create view projects_modules_scope (
	cabal,
	module_id
) as
select pdbs.cabal, m.id
from projects_deps as pdbs, modules as m
where
	(m.package_name == pdbs.package_name) and
	(m.package_version == pdbs.package_version) and
	m.exposed
union
select p.cabal, m.id
from projects as p, modules as m
where (m.cabal == p.cabal)
union
select null, m.id
from modules as m, latest_packages as ps
where
	(m.package_name == ps.package_name) and
	(m.package_version == ps.package_version) and
	m.exposed and
	(ps.package_db in ('user-db', 'global-db'));

-- symbols
create table symbols (
	id integer primary key autoincrement,
	name text not null,
	module_id integer not null, -- definition module
	docs text,
	line integer, -- line of definition
	column integer, -- column of definition
	what text not null, -- kind of symbol: function, method, ...
	type text, -- type of function/method/...
	parent text, -- parent of selector/method/...
	constructors json, -- list of constructors for selector
	args json, -- list of arguments for types
	context json, -- list of contexts for types
	associate text, -- associates for families
	pat_type text,
	pat_constructor text
);

create unique index symbols_id_index on symbols (id);
create unique index symbols_index on symbols (module_id, name, what);

-- modules
create table modules (
	id integer primary key autoincrement,
	-- source module location
	file text, -- source file
	cabal text, -- related project's `.cabal`
	-- installed module location
	install_dirs json, -- list of paths
	package_name text, -- package name of module
	package_version text, -- package version
	installed_name text, -- if not null, should be equal to name
	exposed integer, -- is module exposed or hidden
	-- some other location
	other_location text, -- anything

	name text,
	docs text,
	fixities json, -- list of fixities
	tags json, -- dict of tags, value not used, tag is set if present; used by hsdev to mark if types was inferred or docs scanned
	inspection_error text,
	inspection_time real,
	inspection_opts json -- list of flags
);

create unique index modules_id_index on modules (id);
create index modules_name_index on modules (name);
create unique index modules_file_index on modules (file) where file is not null;
create unique index modules_installed_index on modules (package_name, package_version, installed_name) where
	package_name is not null and
	package_version is not null and
	installed_name is not null;
create unique index modules_other_locations_index on modules (other_location) where other_location is not null;

-- module import statements
create table imports (
	module_id integer not null,
	line integer not null, -- line number of import
	column integer not null, -- column of impoty
	module_name text not null, -- import module name
	qualified integer not null, -- is import qualified
	alias text, -- imported with `as`
	hiding integer, -- is list hiding
	import_list json, -- list of import specs, null if not specified
	import_module_id -- `id` of imported module
);

create unique index imports_position_index on imports (module_id, line, column);

-- symbols bringed into scope by import, with qualifier
create view imported_scopes as
select i.*, q.value as qualifier, s.name as name, s.id as symbol_id
from imports as i, json_each(case when i.qualified then json_array(ifnull(i.alias, i.module_name)) else json_array(ifnull(i.alias, i.module_name), null) end) as q, symbols as s, exports as e
where
	e.module_id == i.import_module_id and
	e.symbol_id == s.id and
	(i.import_list is null or (s.name in (select json_extract(import_list_item.value, '$.name') from json_each(i.import_list) as import_list_item) == not i.hiding));

-- module exports
create table exports (
	module_id integer not null,
	symbol_id integer not null
);

create index exports_module_id_index on exports (module_id);

-- source file module's symbols in scope
create table scopes (
	module_id integer not null,
	qualifier text,
	name text not null,
	symbol_id integer not null
);

create index scopes_name_index on scopes (module_id, name);

-- like `scopes`, but with column `completion` with fully qualified name
create view completions (
	module_id,
	completion,
	qualifier,
	symbol_id
) as
select id, (case when sc.qualifier is null then sc.name else sc.qualifier || '.' || sc.name end) as full_name, sc.qualifier, sc.symbol_id
from modules as m, scopes as sc
where (m.id == sc.module_id);

-- resolved names in module
create table names (
	module_id integer not null,
	qualifier text, -- name qualifier
	name text not null, -- name
	line integer not null, -- line of name
	column integer not null, -- column of name
	line_to integer not null, -- line of name end
	column_to integer not null, -- column of name end
	def_line integer, -- name definition line, for local names
	def_column integer, -- name definition column, for local names
	inferred_type text, -- inferred name type, set by hsdev
	resolved_module text, -- resolved module name, for global and top-level names
	resolved_name text, -- resolved name, for global and top-level names
	resolved_what text, -- resolved symbol kind
	resolve_error text,
	symbol_id integer -- resolved symbol id
);

create unique index names_position_index on names (module_id, line, column, line_to, column_to);
create index names_name_index on names (module_id, name);

-- like `names`, but with definition position set for both local and global names
create view definitions (
	module_id,
	name,
	line,
	column,
	line_to,
	column_to,
	def_module_id,
	def_line,
	def_column,
	local
) as
select module_id, name, line, column, line_to, column_to, module_id, def_line, def_column, 1
from names
where def_line is not null and def_column is not null
union
select n.module_id, n.resolved_name, n.line, n.column, n.line_to, n.column_to, s.module_id, s.line, s.column, 0
from names as n, modules as srcm, modules as m, symbols as s
where
	(n.module_id == srcm.id) and
	(n.symbol_id == s.id) and
	(m.name == n.resolved_module) and
	(s.module_id == m.id) and
	(s.name == n.resolved_name);

-- sources dependencies relations
create view sources_depends (
	module_id,
	module_file,
	depends_id,
	depends_file
) as
select m.id, m.file, im.id, im.file
from modules as im, imports as i, modules as m, projects_modules_scope as ps
where
	(m.cabal is ps.cabal) and
	(ps.module_id == im.id) and
	(i.module_id == m.id) and
	(im.name == i.module_name) and
	(m.file is not null) and
	(im.file is not null);

-- expressions types
create table types (
	module_id integer not null,
	line integer not null, -- line of expression
	column integer not null, -- column of expression
	line_to integer not null, --  line of expression end
	column_to integer not null, -- column of expression end
	expr text not null, -- expression contents
	type text not null -- expression type
);

create unique index types_position_index on types (module_id, line, column, line_to, column_to);

-- modified file contents, hsdev will use it in commands whenever it is newer than file
create table file_contents (
	file text not null, -- file path
	contents text not null, -- file contents
	mtime real not null -- posix modification time
);

create unique index file_contents_index on file_contents (file);

-- table with last time of loading module
create table load_times (
	module_id integer not null, -- module affected
	load_time real not null -- timestamp
);

create unique index load_times_module_id_index on load_times (module_id);

-- table with ghc warnings on file
-- sequential call to load module won't actually reload file and won't produce warnings
-- but we want to see them again
create table messages (
	module_id integer not null,
	line integer not null,
	column integer not null,
	line_to integer not null,
	column_to integer not null,
	severity text,
	message text not null,
	suggestion text
);

create index messages_module_id_index on messages (module_id);
