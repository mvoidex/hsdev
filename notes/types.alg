record database : set where
	modules : dict module-location inspected-module
	projects : dict path project
	project-infos : dict (maybe project) (package-db-stack × list module-location)
	package-dbs : dict package-db (list module-package)
	packages : dict module-package (list module-location)

data module-location : set where
	file-module : module-location where
		file : path
		project : maybe project
	installed-module : module-location where
		install-dirs : list path
		package : maybe module-package
		name : string
	other-location : module-location where
		name : string
	no-location : module-location

data inspected-module : set where
	inspected-module : module-location × tag × module → inspected-module

path : set
path = string

record project : set where
	name : string
	path : path
	cabal : path
	description : maybe project-description

record project-description : set where
	version : string
	lib : maybe library
	exes : list executable
	tests : list test

record library : set where
	modules : list (list string)
	build-info : info

record executable : set where
	name : string
	path : path
	build-info : info

record test : set where
	name : string
	enabled : bool
	main : maybe path
	build-info : info

record info : set where
	depends : list string
	lang : maybe language
	exts : list extension
	opts : list string
	src-dirs : list path
	other-modules : list (list string)

package-db-stack : set
package-db-stack = list package-db

data package-db : set where
	global-db : package-db
	user-db : package-db
	package-db : path → package-db

record module : set where
	id : module-id
	docs : maybe string
	exports : list symbol
	fixities : list fixity
	scope : dict name (list symbol)
	source : maybe parsed

record module-id : set where
	name : string
	location : module-location

record symbol : set where
	id : symbol-id
	docs : maybe string
	pos : maybe position
	info : symbol-info

record symbol-id : set where
	name : string
	module : module-id

data symbol-info : set where
	function : symbol-info where
		type : maybe string
	method : symbol-info where
		type : maybe string
		parent : string
	selector : symbol-info where
		type : maybe string
		parent : string
		constructors : list string
	type : symbol-info where
		args : list string
		context : list string
	newtype : symbol-info where
		args : list string
		context : list string
	data : symbol-info where
		args : list string
		context : list string
	class : symbol-info where
		args : list string
		context : list string
	type-family : symbol-info where
		args : list string
		context : list string
		associate : maybe string
	data-family : symbol-info where
		args : list string
		context : list string
		associate : maybe string
	pat-constructor : symbol-info where
		args : list string
		pat-type : maybe string
	pat-selector : symbol-info where
		type : maybe string
		pat-type : maybe string
		constructor : string

record module-package : set where
	name : text
	version : text
