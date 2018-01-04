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
