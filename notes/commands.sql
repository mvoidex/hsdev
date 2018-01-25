# Find rare used symbols
select
	n.symbol_id,
	n.resolved_module,
	n.resolved_name,
	n.resolved_what,
	count(*) as use_count,
	sum(n.module_id == s.module_id) as internal_usages,
	sum(n.module_id != s.module_id) as external_usages
from names as n, symbols as s
where
	(n.module_id in (select id from modules where cabal like '%hsdev.cabal')) and
	s.id = n.symbol_id and
	(s.module_id in (select id from modules where cabal like '%hsdev.cabal')) and
	n.resolved_what = 'function'
group by
	n.symbol_id,
	n.resolved_module,
	n.resolved_name,
	n.resolved_what
order by external_usages, internal_usages
limit 30;

# Find unused import list items
select distinct
	isc.module_name,
	json_extract(il.value, '$.name') as il_name,
	sum((select count(*) from names as n where n.module_id == 2565 and n.qualifier is isc.qualifier and n.name == isc.name))
from imported_scopes as isc, json_each(isc.import_list) as il
where
	isc.module_id == 2565 and
	il_name == isc.name
group by isc.module_name, il_name;

# Find unused imports
select distinct
	isc.module_name,
	sum((select count(*) from names as n where n.module_id == 2565 and n.qualifier is isc.qualifier and n.name == isc.name))
from imported_scopes as isc
where
	isc.module_id == 2565
group by isc.module_name;


# Find usages by location
select n.* from names as n, modules as mu, symbols as s, modules as m, projects_modules_scope as ps, names as defn, modules as srcm where
	n.module_id = mu.id and
	n.resolved_module = defn.resolved_module and
	n.resolved_name = defn.resolved_name and
	s.name = defn.resolved_name and
	s.module_id = m.id and
	m.name = defn.resolved_module and
	(m.id = srcm.id or m.id = ps.module_id) and
	(((ps.cabal is null) and (srcm.cabal is null)) or (ps.cabal = srcm.cabal)) and
	defn.module_id = srcm.id and
	defn.line == 74 and
	defn.column == 17 and
	(mu.cabal = srcm.cabal or mu.id = srcm.cabal) and
	srcm.file like '%Deps.hs';

# Find local usages by location
select n.* from names as n, names as defn, modules as srcm where
	n.module_id = srcm.id and
	n.def_line = defn.def_line and
	n.def_column = defn.def_column and
	defn.module_id = srcm.id and
	defn.line == 86 and
	defn.column == 2 and
	srcm.file like '%Deps.hs';
