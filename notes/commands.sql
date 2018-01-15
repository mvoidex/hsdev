# Find rare used symbols
select
	resolved_module,
	resolved_name,
	count(*),
	sum(m.name != resolved_module),
	max(s.id in (select e.symbol_id from exports as e where e.module_id == m.id))
from names as n, symbols as s, modules as m
where
	s.module_id == m.id and
	m.cabal like '%hsdev.cabal' and
	m.name == resolved_module and
	s.name == resolved_name and
	s.what == 'function' and
	n.module_id in (select id from modules where cabal like '%hsdev.cabal')
group by
	resolved_module,
	resolved_name
order by count(*)
limit 10;

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
