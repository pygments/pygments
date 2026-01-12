"""
	pygments.lexers.fsmlang
	~~~~~~~~~~~~~~~~~~~~~~~

	Lexer for FSMLang.

	:copyright: Copyright 2025 by the FSMLang Maintainers (see GitHub)
	:license: BSD, see LICENSE for details
"""

import re
import os
from pygments import lex
from pygments.lexer import RegexLexer, include, default, bygroups
from pygments.token import *

__all__ = ['FSMLangLexer']

class FSMLangLexer(RegexLexer):
	"""
	For FSMLang.

	FSMLang allows one to design a Finite State Machine, abstracted from its implementation.
	"""

	name = 'FSMLang'
	aliases = ['fsmlang', 'fsm']
	filenames = ['*.fsm', '*.fsms']
	url = 'https://fsmlang.github.io/'
	version_added = '2.20'

	events = []
	states = []
	fns    = []

	def add_event(lexer, match):
		if match.group(1) not in lexer.events:
			lexer.events.append(match.group(1))

		yield match.start(1), Name.Variable, match.group(1)

	def add_state(lexer, match):
		if match.group(1) not in lexer.states:
			lexer.states.append(match.group(1))

		yield match.start(1), Name.Variable, match.group(1)

	def add_action(lexer, match):
		if match.group(1) not in lexer.fns:
			lexer.fns.append(match.group(1))

		yield match.start(1), Name.Function, match.group(1)

	def check_state(lexer, match):
		if match.group(1) in lexer.states:
			token = Name.Variable
		else:
			token = Error

		yield match.start(1), token, match.group(1)

	def check_transition_type(lexer, match):
		token = Name.Variable
		if match.group(1) not in lexer.states:
			token = Name.Function
			if match.group(1) not in lexer.fns:
				lexer.fns.append(match.group(1))

		yield match.start(1), token, match.group(1)

	def check_function(lexer, match):
		if match.group(1) in lexer.fns:
			token = Name.Function
		else:
			token = Error

		yield match.start(1), token, match.group(1)

	flags = re.DOTALL | re.MULTILINE

	keywords = (
		'actions', 'return', 'returns', 'states', 'events', 'void', 'transition', 'data', 'native',
		'implementation', 'impl', 'on', 'entry', 'exit', 'prologue', 'epilogue', 'translator', 'all',
		'struct', 'union', 'inhibits', 'submachines', 'parent', 'void', 'external', 'reentrant',
		'noEvent', 'noTransition', 'sequence', 'start', 'include', 'guard'
		)

	operators = ( '::' )

	tokens = {
		# General purpose stuff.
		# This was borrowed from one of the other lexers (pl.py?).
		#  It may not all be germane.
      'commentsandwhitespace': [
            (r'\s+', Whitespace),
            (r'/\*\*.*?\*/', Comment.Special),
            (r'//.*?\n', Comment.Single),
            (r'/\*.*?\*/', Comment.Multiline),
				(r'#.*?\n', Comment.Preprosessor),
      ],
		# General purpose stuff.
		# This was borrowed from one of the other lexers (pl.py?).
		#  It may not all be germane.
      'slashstartsregex': [
            include('commentsandwhitespace'),
            (r'/(\\.|[^[/\\\n]|\[(\\.|[^\]\\\n])*])+/'
             r'([gim]+\b|\B)', String.Regex, '#pop'),
            (r'/', Operator, '#pop'),
            default('#pop')
      ],
		'read_sequence': [
         include('commentsandwhitespace'),
			(r',', Operator),
			(r';', Operator, '#pop'),
			(r'(start)(\s+)([$a-zA-Z_][\w\\]*)', bygroups(Keyword.Reserved, Whitespace, Name.Variable)),
			(r'[$a-zA-Z_][\w\\]*', Name.Variable),
		],
		'check_event_name': [
         include('commentsandwhitespace'),
			(r'([$a-zA-Z_][\w\\]*)', Name.Variable, '#pop'),
         default('#pop')
		],
		'namespace': [
         include('commentsandwhitespace'),
			(r'::', Operator,'check_event_name'),	
         default('#pop')
		],
		# Events can have data and other complications.
		'e_name': [
         include('commentsandwhitespace'),
			(r',', Punctuation),
			(r';', Punctuation, '#pop'),

			# Data
			(r'(data)\b', Keyword.Reserved, 'event_data'),

			# Sub-machines can share events from parent.
			(r'parent', Keyword.Reserved,'namespace'),

			# Events must be unique; also, we need to know
         #  them to properly lex some other constructions.
			(r'([$a-zA-Z_][\w\\]*)', add_event),

         default('#pop')
		],
		# States can have entry and exit functions
		's_name': [
         include('commentsandwhitespace'),
			(r'(on)\b', Keyword.Reserved),
			(r'(entry|exit)\b', Keyword.Reserved, 'get_function_name'),

			# Events must be unique; also, we need to know
         #  them to properly lex some other constructions.
			(r'([$a-zA-Z_][\w\\]*)', add_state),
			(r',', Punctuation),
			(r';', Punctuation, '#pop'),
         default('#pop')
		],
		# A machine name.  Should be added to a list for uniqueness.
		'm_name': [
         include('commentsandwhitespace'),
			(r'[$a-zA-Z_][\w\\]*', Name.Variable, '#pop'),
         default('#pop')
		],
		# Transitions can either be states or functions.
		'get_transition_name': [
         include('commentsandwhitespace'),
			(r'([$a-zA-Z_][\w\\]*)', check_transition_type, '#pop'),

         default('#pop')
		],
		# Function names are not always required.
		'get_function_name': [
         include('commentsandwhitespace'),

			# When lexing a state. This is what we'll see if the function
			#  name is omitted, but the other (entry|exit) function is indicated.
			(r'(on)\b', Keyword.Reserved),
			(r'(entry|exit)\b', Keyword.Reserved),

			# Here is a function name!
			(r'[$a-zA-Z_][\w\\]*', Name.Function, '#pop'),

			#This is what we'll see if no function name is given, and no other
			# keywords are used.
			(r';', Punctuation, '#pop'),
			(r',', Punctuation, '#pop'),

         default('#pop')
		],
		# Part of the event/state matrix.
		# A vector is a comma separated list of things, surrounded by
		#  parentheses.
		'get_event_vector': [
         include('commentsandwhitespace'),
			(r'(all)(\s+)?([)])', bygroups(Keyword.Reserved, Whitespace, Punctuation), '#pop'),
			(r'([$a-zA-Z_][\w\\]*)', Name.Variable),
			(r',', Punctuation),
			(r'[)]', Punctuation, '#pop'),
		],
		# Part of the event/state matrix.
		'get_event_vector_or_scalar': [
         include('commentsandwhitespace'),

			# Seeing no opening parenthesis, we have a scalar.
			(r'all', Keyword.Reserved, '#pop'),
			(r'([$a-zA-Z_][\w\\]*)', Name.Variable, '#pop'),

			# Otherwise, we have a vector.
			(r'[(]', Punctuation, 'get_event_vector'),

         default('#pop')
		], 
		# Part of the event/state matrix.
		# A vector is a comma separated list of things, surrounded by
		#  parentheses.
		'get_state_vector': [
         include('commentsandwhitespace'),
			(r'(all)(\s+)?([)])', bygroups(Keyword.Reserved, Whitespace, Punctuation), '#pop'),
			(r'([$a-zA-Z_][\w\\]*)', check_state),
			(r',', Punctuation),
			(r'[)]', Punctuation, '#pop'),
		],
		# Part of the event/state matrix.
		'get_state_vector_or_scalar': [
         include('commentsandwhitespace'),

			# Seeing no opening parenthesis, we have a scalar.
			(r'all', Keyword.Reserved, '#pop'),
			(r'([$a-zA-Z_][\w\\]*)', check_state, '#pop'),

			# Otherwise, we have a vector.
			(r'[(]', Punctuation, 'get_state_vector'),

         default('#pop')
		], 
		'matrix_dfn': [
         include('commentsandwhitespace'),
			(r'\[', Punctuation, 'get_event_vector_or_scalar'),
			(r',', Punctuation, 'get_state_vector_or_scalar'),
			(r'\]', Punctuation),
		],
		# Grab the event/state matrix, which indicates the event
		#  and state combinations in which an action and/or
		#  transition should be taken.
		'matrix': [
			include('matrix_dfn'),
         default('#pop')
		],
		# Grab the action function name.  Unlike events and states, these
		#   do not need to be unique.
		'a_name': [
         include('commentsandwhitespace'),
			(r'(transition)\b', Keyword.Reserved, 'get_transition_name'),
			(r'(guard)\b', Keyword.Reserved, 'get_transition_name'),
			(r'([$a-zA-Z_][\w\\]*)', add_action, 'matrix'),
			(r';', Punctuation, '#pop'),
         default('#pop')
		],
		'data': [
         include('commentsandwhitespace'),
			(r'[{]', Punctuation, 'brace_enclosed'),
			default('#pop')
		],
		'brace_enclosed': [
         include('commentsandwhitespace'),
			(r'[}]', Punctuation, '#pop'),
			(r'[^{}]+', Text),
			(r'[{]', Punctuation, '#push'),
		],
		'event_data': [
			(r'translator', Keyword.Reserved, 'get_function_name'),
			include('data')
		],
		'native': [
         include('commentsandwhitespace'),
			(r'[{]', Punctuation, 'brace_enclosed'),
			(r'[^{}]+', Text),
			default('#pop')
		],
		'returns': [
         include('commentsandwhitespace'),
			(r'(noEvent|noTransition)\b', Keyword.Reserved),
			(r'(parent)(\s+)?([:]{2})', bygroups(Keyword.Reserved, Whitespace, Operator)),
			(r'([$a-zA-Z_][\w\\]*)(\s+)?([:]{2})', bygroups(Name.Variable, Whitespace, Operator)),
			(r'([$a-zA-Z_][\w\\]*)', Name.Variable),
			(r',', Punctuation),
			default('#pop')
		],
		'transition': [
			include('matrix_dfn'),
			(r'([$a-zA-Z_][\w\\]*)', check_transition_type, '#pop'),
			(r';', Punctuation, '#pop')
      ],
		'root': [
			(r'(include)(\s+)(")([^"]+)(")'
				, bygroups(Keyword.Reserved
								, Whitespace
								, Punctuation
								, Text
								, Punctuation
								)
			),
			(r'(include)(\s+)(\b\w+\.*\w*\b)'
				, bygroups(Keyword.Reserved
								, Whitespace
								, Text
								)
			),
			(r'^(?=\s|/)', Whitespace, 'slashstartsregex'),
			include('commentsandwhitespace'),
			(r'(machine)\b', Keyword.Declaration, 'm_name'),
			(r'(event)\b', Keyword.Declaration, 'e_name'),
			(r'(state)\b', Keyword.Declaration, 's_name'),
			(r'(action)\b', Keyword.Declaration, 'a_name'),
			(r'(transition)\b', Keyword.Declaration, 'transition'),
			(r'(guard)\b', Keyword.Declaration, 'transition'),
			(r'(returns)\b', Keyword.Reserved, 'returns'),
			(r'(sequence)\b', Keyword.Declaration, 'read_sequence'),
			(r'(native)([{])'
				, bygroups(Keyword.Reserved, Punctuation)
				, 'native'),
			(r'(native)(\s+)(prologue)([{])'
				, bygroups(Keyword.Reserved, Whitespace, Keyword.Reserved, Punctuation)
				, 'native'),
			(r'(native)(\s+)(epilogue)([{])'
				, bygroups(Keyword.Reserved, Whitespace, Keyword.Reserved, Punctuation)
				, 'native'),
			(r'(native)(\s+)([{])'
				, bygroups(Keyword.Reserved, Whitespace, Punctuation)
				, 'native'),
			(r'(native)(\s+)(prologue)(\s+)([{])'
				, bygroups(Keyword.Reserved, Whitespace, Keyword.Reserved, Whitespace, Punctuation)
				, 'native'),
			(r'(native)(\s+)(epilogue)(\s+)([{])'
				, bygroups(Keyword.Reserved, Whitespace, Keyword.Reserved, Whitespace, Punctuation)
				, 'native'),
			(r'(native)(\s+)(impl)([{])'
				, bygroups(Keyword.Reserved, Whitespace, Keyword.Reserved, Punctuation)
				, 'native'),
			(r'(native)(\s+)(impl)(\s+)(prologue)([{])'
				, bygroups(Keyword.Reserved, Whitespace, Keyword.Reserved, Whitespace, Keyword.Reserved, Punctuation)
				, 'native'),
			(r'(native)(\s+)(impl)(\s+)(epilogue)([{])'
				, bygroups(Keyword.Reserved, Whitespace, Keyword.Reserved, Whitespace, Keyword.Reserved, Punctuation)
				, 'native'),
			(r'(native)(\s+)(impl)(\s+)([{])'
				, bygroups(Keyword.Reserved, Whitespace, Keyword.Reserved, Whitespace, Punctuation)
				, 'native'),
			(r'(native)(\s+)(impl)(\s+)(prologue)(\s+)([{])'
				, bygroups(Keyword.Reserved, Whitespace, Keyword.Reserved, Whitespace, Keyword.Reserved, Whitespace, Punctuation)
				, 'native'),
			(r'(native)(\s+)(impl)(\s+)(epilogue)(\s+)([{])'
				, bygroups(Keyword.Reserved, Whitespace, Keyword.Reserved, Whitespace, Keyword.Reserved, Whitespace, Punctuation)
				, 'native'),
			(r'(native)(\s+)(implementation)([{])'
				, bygroups(Keyword.Reserved, Whitespace, Keyword.Reserved, Punctuation)
				, 'native'),
			(r'(native)(\s+)(implementation)(\s+)(prologue)([{])'
				, bygroups(Keyword.Reserved, Whitespace, Keyword.Reserved, Whitespace, Keyword.Reserved, Punctuation)
				, 'native'),
			(r'(native)(\s+)(implementation)(\s+)(epilogue)([{])'
				, bygroups(Keyword.Reserved, Whitespace, Keyword.Reserved, Whitespace, Keyword.Reserved, Punctuation)
				, 'native'),
			(r'(native)(\s+)(implementation)(\s+)([{])'
				, bygroups(Keyword.Reserved, Whitespace, Keyword.Reserved, Whitespace, Punctuation)
				, 'native'),
			(r'(native)(\s+)(implementation)(\s+)(prologue)(\s+)([{])'
				, bygroups(Keyword.Reserved, Whitespace, Keyword.Reserved, Whitespace, Keyword.Reserved, Whitespace, Punctuation)
				, 'native'),
			(r'(native)(\s+)(implementation)(\s+)(epilogue)(\s+)([{])'
				, bygroups(Keyword.Reserved, Whitespace, Keyword.Reserved, Whitespace, Keyword.Reserved, Whitespace, Punctuation)
				, 'native'),
			(r'(data)', Keyword.Reserved, 'data'),
			(r'(on)(\s+)(transition)', bygroups(Keyword.Reserved, Whitespace, Keyword.Reserved), 'get_function_name'),
			(r'({})\b'.format('|'.join(keywords)), Keyword.Reserved),
			(r'({})\b'.format('|'.join(operators)), Operator),
			(r'::', Operator),
			(r'[{\[;,]', Punctuation, 'slashstartsregex'),
			(r'[}\]]', Punctuation),
			(r'([$a-zA-Z_][\w\\]*)\b', check_function),
		]

	}

