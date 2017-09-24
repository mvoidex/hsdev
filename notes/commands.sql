-- usages
select m.file, n.line, n.column
from modules as m, names as n
where
	(m.id == n.module_id) and
	(n.resolved_module == 'Data.Group') and
	(n.resolved_name == 'add');

-- whoat
select mdef.name, s.name, s.what
from modules as m, names as n, modules as mdef, symbols as s
where
	(m.id == n.module_id) and
	(mdef.id == s.module_id) and
	(mdef.name == n.resolved_module) and
	(s.name == n.resolved_name) and
	(m.file == 'D:\users\voidex\Documents\Projects\hsdev\src\Data\Group.hs') and
	((28, 13) between (n.line, n.column) and (n.line_to, n.column_to));

-- unresolved symbols
select n.qualifier, n.name, n.line, n.column
from modules as m, names as n
where
	(m.id == n.module_id) and
	(m.file == 'D:\users\voidex\Documents\Projects\hsdev\src\HsDev\Client\Commands.hs') and
	(n.resolve_error is not null);

-- whois
select s.name, mdef.name
from modules as m, modules as mdef, scopes as sc, symbols as s
where
	(m.id == sc.module_id) and
	(s.id == sc.symbol_id) and
	(s.module_id == mdef.id) and
	(m.file == 'D:\users\voidex\Documents\Projects\hsdev\src\Data\Group.hs') and
	(sc.qualifier == 'M') and
	(sc.name == 'unionWith');

-- lookup
select s.name, m.name
from projects as p, projects_deps as pdeps, modules as m, symbols as s
where
	(p.id == pdeps.project_id) and
	(m.cabal == p.cabal or m.package_name == pdeps.package_name) and
	(s.module_id == m.id) and
	(p.name == 'hsdev') and
	(s.name == 'unionWith');

-- complete
select c.completion
from modules as m, completions as c
where
	(m.id == c.module_id) and
	(m.file == 'D:\users\voidex\Documents\Projects\hsdev\src\Data\Group.hs') and
	(c.completion like 'Data.Lis%');



-- notes

-- latest packages in package_db_stack of project
select p.package_db, p.package_name, max(p.package_version)
from projects as p, json_each(p.package_db_stack) as pdbs, package_dbs as pdb
where (pdb.package_db == pdbs.value)
group by p.package_name
order by p.package_name;
