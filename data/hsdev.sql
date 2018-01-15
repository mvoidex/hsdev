pragma case_sensitive_like = true;

create table hsdev (
	option text,
	value json
);

create table package_dbs (
	package_db text, -- global, user or path
	package_name text,
	package_version text
);

create view latest_packages (
	package_db,
	package_name,
	package_version
) as
select package_db, package_name, max(package_version)
from package_dbs
group by package_db, package_name;

create table projects (
	id integer primary key autoincrement,
	name text,
	cabal text,
	version text,
	package_db_stack json -- list of package-db
);

create unique index projects_id_index on projects (id);

create table libraries (
	project_id integer,
	modules json, -- list of modules
	build_info_id integer
);

create table executables (
	project_id integer,
	name text,
	path text,
	build_info_id integer
);

create table tests (
	project_id integer,
	name text,
	enabled integer,
	main text,
	build_info_id integer
);

create view targets (
	project_id,
	build_info_id
) as
select project_id, build_info_id from libraries
union
select project_id, build_info_id from executables
union
select project_id, build_info_id from tests;

create table build_infos(
	id integer primary key autoincrement,
	depends json, -- list of dependencies
	language text,
	extensions json, -- list of extensions
	ghc_options json, -- list of ghc-options
	source_dirs json, -- list of source directories
	other_modules json -- list of other modules
);

create view projects_deps (
	cabal,
	package_name,
	package_version
) as
select distinct p.cabal, deps.value, ps.package_version
from projects as p, json_each(p.package_db_stack) as pdb_stack, build_infos as b, json_each(b.depends) as deps, targets as t, latest_packages as ps
where (p.id == t.project_id) and (b.id == t.build_info_id) and (deps.value <> p.name) and (ps.package_name == deps.value) and (ps.package_db == pdb_stack.value);

create view projects_modules_scope (
	cabal,
	module_id
) as
select pdbs.cabal, m.id
from projects_deps as pdbs, modules as m
where (m.package_name == pdbs.package_name) and (m.package_version == pdbs.package_version)
union
select p.cabal, m.id
from projects as p, modules as m
where (m.cabal == p.cabal)
union
select null, m.id
from modules as m, package_dbs as ps
where (m.package_name == ps.package_name) and (m.package_version == ps.package_version) and (ps.package_db in ('user-db', 'global-db'));

create unique index build_infos_id_index on build_infos (id);

create table symbols (
	id integer primary key autoincrement,
	name text,
	module_id integer,
	docs text,
	line integer,
	column integer,
	what text, -- kind of symbol: function, method, ...
	type text,
	parent text,
	constructors json, -- list of constructors for selector
	args json, -- list of arguments for types
	context json, -- list of contexts for types
	associate text, -- associates for families
	pat_type text,
	pat_constructor text
);

create unique index symbols_id_index on symbols (id);
create index symbols_module_id_index on symbols (module_id);
create index symbols_name_index on symbols (name);

create table modules (
	id integer primary key autoincrement,
	file text,
	cabal text,
	-- project_id integer,
	install_dirs json, -- list of paths
	package_name text,
	package_version text,
	installed_name text, -- if not null, should be equal to name
	other_location text,

	name text,
	docs text,
	fixities json, -- list of fixities
	tags json, -- dict of tags, value not used, tag is set if present
	inspection_error text,
	inspection_time integer,
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

create table imports (
	module_id integer,
	line integer,
	module_name text,
	qualified integer,
	alias text,
	hiding integer,
	import_list json -- list of import specs
);

create index imports_module_id_index on imports (module_id);

create view imported_modules as
select i.*, im.id as imported_id
from imports as i, projects_modules_scope as ps, modules as im, modules as m
where
	i.module_id == m.id and
	((ps.cabal is null and m.cabal is null) or (ps.cabal == m.cabal)) and
	ps.module_id == im.id and
	im.name == i.module_name;

create view imported_scopes as
select i.*, q.value as qualifier, s.name as name, s.id as symbol_id
from imported_modules as i, json_each(case when i.qualified then json_array(ifnull(i.alias, i.module_name)) else json_array(ifnull(i.alias, i.module_name), null) end) as q, symbols as s, exports as e
where
	e.module_id == i.imported_id and
	e.symbol_id == s.id and
	(i.import_list is null or (s.name in (select json_extract(import_list_item.value, '$.name') from json_each(i.import_list) as import_list_item) == not i.hiding));

create table exports (
	module_id integer,
	symbol_id integer
);

create index exports_module_id_index on exports (module_id);

create table scopes (
	module_id integer,
	qualifier text,
	name text,
	symbol_id integer
);

create index scopes_module_id_index on scopes (module_id);

create view completions (
	module_id,
	completion,
	symbol_id
) as
select id, (case when sc.qualifier is null then sc.name else sc.qualifier || '.' || sc.name end) as full_name, sc.symbol_id
from modules as m, scopes as sc
where (m.id == sc.module_id);

create table names (
	module_id integer,
	qualifier text,
	name text,
	line integer,
	column integer,
	line_to integer,
	column_to integer,
	def_line integer,
	def_column integer,
	inferred_type text,
	resolved_module text,
	resolved_name text,
	resolve_error text
);

create unique index names_position_index on names (module_id, line, column, line_to, column_to);
create index names_module_id_index on names (module_id);

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
from names as n, modules as srcm, modules as m, projects_modules_scope as ps, symbols as s
where
	(n.module_id == srcm.id) and
	(srcm.cabal == ps.cabal) and
	(m.id == ps.module_id) and
	(m.name == n.resolved_module) and
	(s.module_id == m.id) and
	(s.name == n.resolved_name);

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

create table types (
	module_id integer,
	line integer,
	column integer,
	line_to integer,
	column_to integer,
	expr text,
	type text
);

create table file_contents (
	file text not null,
	contents text,
	mtime integer
);
