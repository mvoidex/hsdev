create table module_locations (
	id integer primary key autoincrement,
	file text,
	cabal text,
	install_dirs text,
	package_name text,
	package_version text,
	other text
);

create table projects (cabal text, version text);
