# -*- coding: utf-8 -*-
"""
    pygments.lexers.ride
    ~~~~~~~~~~~~~~~~~~~

    Lexer for the Ride programming language.

    :copyright: Copyright 2006-2017 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, words, include
from pygments.token import Comment, Keyword, Name, Number, Punctuation, String, Text

__all__ = ['RideLexer']


class RideLexer(RegexLexer):
    """
    For `Ride <https://docs.wavesplatform.com/en/ride/about-ride.html>`_ source code.

    .. versionadded:: 0.1
    """

    name = 'Ride'
    aliases = ['ride']
    filenames = ['*.ride']
    mimetypes = ['text/x-ride']

    validName = r'[a-z_][a-zA-Z0-9_\']*'

    builtinOps = (
        '||', '|', '>=', '>', '==',
        '=', '<=', '::', ':', '!=', '/',
        '.', '->', '-', '+', '*', '&&', '%',
    )

    typesName = (
  'Unit',
  'Int',
  'Boolean',
  'ByteVector',
  'String',
  'Address',
  'Alias',
  'Transfer',
  'AssetPair',
  'DataEntry',
  'Order',
  'Transaction',
  'GenesisTransaction',
  'PaymentTransaction',
  'ReissueTransaction',
  'BurnTransaction',
  'MassTransferTransaction',
  'ExchangeTransaction',
  'TransferTransaction',
  'SetAssetScriptTransaction',
  'InvokeScriptTransaction',
  'IssueTransaction',
  'LeaseTransaction',
  'LeaseCancelTransaction',
  'CreateAliasTransaction',
  'SetScriptTransaction',
  'SponsorFeeTransaction',
  'DataTransaction',
  'WriteSet',
  'AttachedPayment',
  'ScriptTransfer',
  'TransferSet',
  'ScriptResult',
  'Invocation',
  'Asset',
  'BlockInfo',
    )

    functionsName = (
  'fraction',
  'size',
  'toBytes',
  'take',
  'drop',
  'takeRight',
  'dropRight',
  'toString',
  'isDefined',
  'extract',
  'throw',
  'getElement',
  'value',
  'cons',
  'ensure',
  'toUtf8String',
  'toInt',
  'indexOf',
  'split',
  'parseInt',
  'parseIntValue',
  'keccak256',
  'blake2b256',
  'sha256',
  'sigVerify',
  'toBase58String',
  'fromBase58String',
  'toBase64String',
  'fromBase64String',
  'transactionById',
  'transactionHeightById',
  'getInteger',
  'getBoolean',
  'getBinary',
  'getString',
  'addressFromPublicKey',
  'addressFromString',
  'addressFromRecipient',
  'assetBalance',
  'wavesBalance',
  'getIntegerValue',
  'getBooleanValue',
  'getBinaryValue',
  'getStringValue',
  'addressFromStringValue',
  'assetInfo',
            )

    reservedWords = words((
        'match', 'case', 'else', 'if',
        'let', 'then'
        ), suffix=r'\b')

    tokens = {
        'root': [

            # Comments
            (r'#.*', Comment.Single),

            # Whitespace
            (r'\s+', Text),

            # Strings
            (r'"', String, 'doublequote'),
            (r'utf8\'', String, 'utf8quote'),
            (r'base(58|64|16)\'', String, 'singlequote'),

            # Keywords
            (reservedWords, Keyword.Reserved),

            # Types
            (words((typesName)), Keyword.Type),

#            # Main
#            (specialName, Keyword.Reserved),

            # Prefix Operators
            (words((builtinOps), prefix=r'\(', suffix=r'\)'), Name.Function),

            # Infix Operators
            (words((builtinOps)), Name.Function),

            (words((functionsName)), Name.Function),

            # Numbers
            include('numbers'),

            # Variable Names
            (validName, Name.Variable),

            # Parens
            (r'[,()\[\]{}]', Punctuation),

        ],

        'comment': [
        ],

        'doublequote': [
            (r'\\u[0-9a-fA-F]{4}', String.Escape),
            (r'\\[nrfvb\\"]', String.Escape),
            (r'[^"]', String),
            (r'"', String, '#pop'),
        ],

        'utf8quote': [
            (r'\\u[0-9a-fA-F]{4}', String.Escape),
            (r'\\[nrfvb\\\']', String.Escape),
            (r'[^\']', String),
            (r'\'', String, '#pop'),
        ],

        'singlequote': [
            (r'[^\']', String),
            (r'\'', String, '#pop'),
        ],

        'numbers': [
            (r'_?\d+', Number.Integer),
        ],
    }
