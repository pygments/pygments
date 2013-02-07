#!/usr/bin/lasso9

/*
	Builtins Generator for Lasso 9

	This is the shell script that was used to extract Lasso 9's built-in keywords
	and generate most of the _lassobuiltins.py file. When run, it creates a file
	named "lassobuiltins-9.py" containing the types, traits, and methods of the
	currently-installed version of Lasso 9.

	A partial list of keywords in Lasso 8 can be generated with this code:

	<?LassoScript
		local('l8tags' = list);
		iterate(tags_list, local('i'));
			#l8tags->insert(string_removeleading(#i, -pattern='_global_'));
		/iterate;
		#l8tags->sort;
		iterate(#l8tags, local('i'));
			string_lowercase(#i)+"<br>";
		/iterate;

*/

output("This output statement is required for a complete list of methods.")
local(f) = file("lassobuiltins-9.py")
#f->doWithClose => {

#f->openWrite
#f->writeString('# -*- coding: utf-8 -*-
"""
    pygments.lexers._lassobuiltins
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Built-in Lasso types, traits, and methods.
"""

')

lcapi_loadModules

// Load all of the libraries from builtins and lassoserver
// This forces all possible available types and methods to be registered
local(srcs =
	tie(
		dir(sys_masterHomePath + 'LassoLibraries/builtins/')->eachFilePath,
		dir(sys_masterHomePath + 'LassoLibraries/lassoserver/')->eachFilePath
	)
)

with topLevelDir in #srcs
where not #topLevelDir->lastComponent->beginsWith('.')
do protect => {
	handle_error => {
		stdoutnl('Unable to load: ' + #topLevelDir + ' ' + error_msg)
	}
	library_thread_loader->loadLibrary(#topLevelDir)
	stdoutnl('Loaded: ' + #topLevelDir)
}

local(
	typesList = list(),
	traitsList = list(),
	methodsList = list()
)

// unbound methods
with method in sys_listUnboundMethods
let m_name = #method->methodName->asString
where not #m_name->endsWith('=')		// skip setter methods
where #m_name->isAlpha(1)		// skip unpublished methods
where #methodsList !>> #m_name
do #methodsList->insert(#m_name)

// traits
with trait in sys_listTraits
let name = #trait->asString
where not #name->beginsWith('$')		// skip combined traits
where #traitsList !>> #name
do {
	#traitsList->insert(#name)
	with method in tie(#trait->getType->provides, #trait->getType->requires)
	let m_name = #method->methodName->asString
	where not #m_name->endsWith('=')		// skip setter methods
	where #m_name->isAlpha(1)		// skip unpublished methods
	where #methodsList !>> #m_name
	do #methodsList->insert(#m_name)
}

// types
with type in sys_listTypes
let name = #type->asString
where not #name->endsWith('$')		// skip threads
where #typesList !>> #name
do {
	#typesList->insert(#name)
	with method in #type->getType->listMethods
	let m_name = #method->methodName->asString
	where not #m_name->endsWith('=')		// skip setter methods
	where #m_name->isAlpha(1)		// skip unpublished methods
	where #methodsList !>> #m_name
	do #methodsList->insert(#m_name)
}

#f->writeString("BUILTINS = {
    'Types': [
")
with t in #typesList
do #f->writeString("        '"+string_lowercase(#t)+"',\n")

#f->writeString("    ],
    'Traits': [
")
with t in #traitsList
do #f->writeString("        '"+string_lowercase(#t)+"',\n")

#f->writeString("    ],
    'Methods': [
")
with t in #methodsList
do #f->writeString("        '"+string_lowercase(#t)+"',\n")

#f->writeString("    ],
}
")

}
