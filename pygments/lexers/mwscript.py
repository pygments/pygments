"""
    pygments.lexers.mwscript
    ~~~~~~~~~~~~~~~~~~~~~~~~~

    Lexer for Morrowind scripting (mwscript), the scripting language used by
    the Elder Scrolls III: Morrowind Construction Set and OpenMW.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, bygroups, words
from pygments.token import Comment, Keyword, Name, Number, Operator, \
    Punctuation, String, Whitespace

__all__ = ['MWScriptLexer']


class MWScriptLexer(RegexLexer):
    """
    For Morrowind scripting (mwscript) source code, the scripting language
    used by the Elder Scrolls III: Morrowind Construction Set and OpenMW.
    """

    name = 'MWScript'
    aliases = ['mwscript', 'morrowind']
    filenames = ['*.mwscript']
    mimetypes = ['text/x-mwscript']
    url = 'https://en.uesp.net/wiki/Morrowind_Mod:Scripting_Basics'
    version_added = '2.21'

    # Morrowind scripting is case-insensitive for keywords and functions.
    flags = re.IGNORECASE | re.MULTILINE

    # Flow-control and structural keywords.
    keywords = (
        'begin', 'end', 'if', 'elseif', 'else', 'endif', 'while', 'endwhile',
        'return', 'set', 'to',
    )

    # Variable type declarations.
    types = ('short', 'long', 'float')

    # Special variables, globals and event locals that scripts can read.
    pseudo = (
        'player', 'playersavegameid', 'companion', 'day', 'dayspassed',
        'gamehour', 'month', 'pcrace', 'pcvampire', 'pcwerewolf', 'pcskipequip',
        'stayoutside', 'onactivate', 'ondeath', 'onknockout', 'onmurder',
        'onpcadd', 'onpcdrop', 'onpcequip', 'onpchitme', 'onpcrepair',
        'onpcsoulgemuse', 'onrepair', 'timescale', 'year',
    )

    # A representative set of built-in script functions and commands.
    builtins = (
        'activate', 'additem', 'addsoulgem', 'addspell', 'addtolevcreature',
        'addtolevitem', 'addtopic', 'aiactivate', 'aiescort', 'aiescortcell',
        'aifollow', 'aifollowcell', 'aitravel', 'aiwander', 'becomewerewolf',
        'betacomment', 'cast', 'cellchanged', 'cellupdate', 'centeroncell',
        'centeronexterior', 'changeweather', 'choice', 'clearforcejump',
        'clearforcemovejump', 'clearforcerun', 'clearforcesneak',
        'clearinfoactor', 'createmaps', 'disable', 'disablelevitation',
        'disableplayercontrols', 'disableplayerfighting',
        'disableplayerjumping', 'disableplayerlooking', 'disableplayermagic',
        'disableplayerviewswitch', 'disableteleporting', 'disablevanitymode',
        'dontsaveobject', 'drop', 'dropsoulgem', 'enable', 'enablebirthmenu',
        'enableclassmenu', 'enableinventorymenu', 'enablelevelupmenu',
        'enablelevitation', 'enablemagicmenu', 'enablemapmenu',
        'enablenamemenu', 'enableplayercontrols', 'enableplayerfighting',
        'enableplayerjumping', 'enableplayerlooking', 'enableplayermagic',
        'enableplayerviewswitch', 'enableracemenu', 'enablerest',
        'enablestatreviewmenu', 'enablestatsmenu', 'enableteleporting',
        'enablevanitymode', 'equip', 'explodespell', 'face', 'fadein',
        'fadeout', 'fadeto', 'fall', 'filljournal', 'fillmap', 'fixme',
        'forcegreeting', 'forcejump', 'forcemovejump', 'forcerun', 'forcesneak',
        'getacrobatics', 'getagility', 'getaipackagedone', 'getalarm',
        'getalchemy', 'getalteration', 'getangle', 'getarmorbonus', 'getarmorer',
        'getarmortype', 'getathletics', 'getattackbonus', 'getattacked',
        'getaxe', 'getblightdisease', 'getblindness', 'getblock',
        'getbluntweapon', 'getbuttonpressed', 'getcastpenalty', 'getchameleon',
        'getcollidingactor', 'getcollidingpc', 'getcommondisease',
        'getconjuration', 'getcurrentaipackage', 'getcurrenttime',
        'getcurrentweather', 'getdeadcount', 'getdefendbonus', 'getdestruction',
        'getdetected', 'getdisabled', 'getdisposition', 'getdistance',
        'geteffect', 'getenchant', 'getendurance', 'getfactionreaction',
        'getfatigue', 'getfight', 'getflee', 'getflying', 'getforcejump',
        'getforcemovejump', 'getforcerun', 'getforcesneak', 'gethandtohand',
        'gethealth', 'gethealthgetratio', 'getheavyarmor', 'gethello',
        'getillusion', 'getintelligence', 'getinterior', 'getinvisible',
        'getitemcount', 'getjournalindex', 'getlevel', 'getlightarmor',
        'getlineofsight', 'getlos', 'getlocked', 'getlongblade', 'getluck',
        'getmagicka', 'getmarksman', 'getmasserphase', 'getmediumarmor',
        'getmercantile', 'getmysticism', 'getparalysis', 'getpccell',
        'getpccrimelevel', 'getpcfacrep', 'getpcinjail', 'getpcjumping',
        'getpcrank', 'getpcrunning', 'getpcsleep', 'getpcsneaking',
        'getpctraveling', 'getpcvisionbonus', 'getpersonality',
        'getplayercontrolsdisabled', 'getplayerfightingdisabled',
        'getplayerjumpingdisabled', 'getplayerlookingdisabled',
        'getplayermagicdisabled', 'getplayerviewswitch', 'getpos', 'getrace',
        'getreputation', 'getresistblight', 'getresistcorprus',
        'getresistdisease', 'getresistfire', 'getresistfrost',
        'getresistmagicka', 'getresistnormalweapons', 'getresistparalysis',
        'getresistpoison', 'getresistshock', 'getrestoration', 'getscale',
        'getsecondspassed', 'getsecundaphase', 'getsecurity', 'getshortblade',
        'getsilence', 'getsneak', 'getsoundplaying', 'getspear',
        'getspeechcraft', 'getspeed', 'getspell', 'getspelleffects',
        'getspellreadied', 'getsquareroot', 'getstandingactor', 'getstandingpc',
        'getstartingangle', 'getstartingpos', 'getstrength', 'getsuperjump',
        'getswimspeed', 'gettarget', 'getunarmored', 'getvanitymodedisabled',
        'getwaterbreathing', 'getwaterlevel', 'getwaterwalking',
        'getweapondrawn', 'getweapontype', 'getwerewolfkills', 'getwillpower',
        'getwindspeed', 'goodbye', 'gotojail', 'hasitemequipped', 'hassoulgem',
        'help', 'hitattemptonme', 'hitonme', 'hurtcollidingactor',
        'hurtstandingactor', 'iswerewolf', 'journal', 'lock', 'loopgroup',
        'lowerrank', 'menumode', 'menutest', 'messagebox', 'modacrobatics',
        'modagility', 'modalarm', 'modalchemy', 'modalteration', 'modarmorbonus',
        'modarmorer', 'modathletics', 'modattackbonus', 'modaxe', 'modblindness',
        'modblock', 'modbluntweapon', 'modcastpenalty', 'modchameleon',
        'modconjuration', 'modcurrentfatigue', 'modcurrenthealth',
        'modcurrentmagicka', 'moddefendbonus', 'moddestruction', 'moddisposition',
        'modenchant', 'modendurance', 'modfactionreaction', 'modfatigue',
        'modfight', 'modflee', 'modflying', 'modhandtohand', 'modhealth',
        'modheavyarmor', 'modhello', 'modillusion', 'modintelligence',
        'modinvisible', 'modlightarmor', 'modlongblade', 'modluck', 'modmagicka',
        'modmarksman', 'modmediumarmor', 'modmercantile', 'modmysticism',
        'modparalysis', 'modpccrimelevel', 'modpcfacrep', 'modpcvisionbonus',
        'modpersonality', 'modregion', 'modreputation', 'modresistblight',
        'modresistcorprus', 'modresistdisease', 'modresistfire',
        'modresistfrost', 'modresistmagicka', 'modresistnormalweapons',
        'modresistparalysis', 'modresistpoison', 'modresistshock',
        'modrestoration', 'modscale', 'modsecurity', 'modshortblade',
        'modsilence', 'modsneak', 'modspear', 'modspeechcraft', 'modspeed',
        'modstrength', 'modsuperjump', 'modswimspeed', 'modunarmored',
        'modwaterbreathing', 'modwaterlevel', 'modwaterwalking', 'modwillpower',
        'move', 'moveonetoone', 'moveworld', 'payfine', 'payfinethief',
        'pcclearexpelled', 'pcexpell', 'pcexpelled', 'pcforce1stperson',
        'pcforce3rdperson', 'pcget3rdperson', 'pcjoinfaction', 'pclowerrank',
        'pcraiserank', 'placeatme', 'placeatpc', 'placeitem', 'placeitemcell',
        'playbink', 'playgroup', 'playloopsound3d', 'playloopsound3dvp',
        'playsound', 'playsound3d', 'playsound3dvp', 'playsoundvp', 'position',
        'positioncell', 'purgetextures', 'raiserank', 'random', 'removeeffects',
        'removefromlevcreature', 'removefromlevitem', 'removeitem',
        'removesoulgem', 'removespell', 'removespelleffects', 'repairedonme',
        'resetactors', 'resurrect', 'rotate', 'rotateworld', 'samefaction',
        'say', 'saydone', 'scriptrunning', 'setacrobatics', 'setagility',
        'setalarm', 'setalchemy', 'setalteration', 'setangle', 'setarmorbonus',
        'setarmorer', 'setathletics', 'setatstart', 'setattackbonus', 'setaxe',
        'setblindness', 'setblock', 'setbluntweapon', 'setcastpenalty',
        'setchameleon', 'setconjuration', 'setdefendbonus', 'setdelete',
        'setdestruction', 'setdisposition', 'setenchant', 'setendurance',
        'setfactionreaction', 'setfatigue', 'setfight', 'setflee', 'setflying',
        'sethandtohand', 'sethealth', 'setheavyarmor', 'sethello',
        'setillusion', 'setintelligence', 'setinvisible', 'setjournalindex',
        'setlevel', 'setlightarmor', 'setlongblade', 'setluck', 'setmagicka',
        'setmarksman', 'setmediumarmor', 'setmercantile', 'setmysticism',
        'setparalysis', 'setpccrimelevel', 'setpcfacrep', 'setpcvisionbonus',
        'setpersonality', 'setpos', 'setreputation', 'setresistblight',
        'setresistcorprus', 'setresistdisease', 'setresistfire',
        'setresistfrost', 'setresistmagicka', 'setresistnormalweapons',
        'setresistparalysis', 'setresistpoison', 'setresistshock',
        'setrestoration', 'setscale', 'setsecurity', 'setshortblade',
        'setsilence', 'setsneak', 'setspear', 'setspeechcraft', 'setspeed',
        'setstrength', 'setsuperjump', 'setswimspeed', 'setunarmored',
        'setwaterbreathing', 'setwaterlevel', 'setwaterwalking',
        'setwerewolfacrobatics', 'setwillpower', 'show', 'showanim', 'showgroup',
        'showmap', 'showrestmenu', 'showscenegraph', 'showtargets', 'showvars',
        'skipanim', 'startcombat', 'startscript', 'stopcelltest', 'stopcombat',
        'stopscript', 'stopsound', 'streammusic', 'testcells',
        'testinteriorcells', 'testmodels', 'testthreadcells', 'toggleai',
        'toggleborders', 'togglecollision', 'togglecollisionboxes',
        'togglecollisiongrid', 'togglecombatstats', 'toggledebugtext',
        'toggledialoguestats', 'togglefogofwar', 'togglefullhelp', 'togglegodmode',
        'togglegrid', 'togglekillstats', 'togglelights', 'toggleloadfade',
        'togglemagicstats', 'togglemenus', 'togglepathgrid', 'togglescriptoutput',
        'togglescripts', 'togglesky', 'togglestats', 'toggletexturestring',
        'togglevanitymode', 'togglewater', 'togglewireframe', 'toggleworld',
        'turnmoonred', 'turnmoonwhite', 'undowerewolf', 'unlock', 'usedonme',
        'wakeuppc', 'xbox',
    )

    tokens = {
        'root': [
            (r'[^\S\n]+', Whitespace),
            (r'\n', Whitespace),
            (r';[^\n]*', Comment.Single),
            # Closing quote optional so an unterminated string (common while
            # editing) is highlighted as a string instead of an Error token.
            (r'"[^"\n]*"?', String.Double),
            # ``begin <name>`` starts a script; highlight the script name.
            (r'\b(begin)\b([^\S\n]+)([\w.]+)',
             bygroups(Keyword, Whitespace, Name.Namespace)),
            (words(keywords, suffix=r'\b'), Keyword),
            (words(types, suffix=r'\b'), Keyword.Type),
            (words(pseudo, suffix=r'\b'), Name.Builtin.Pseudo),
            (words(builtins, suffix=r'\b'), Name.Builtin),
            (r'->|==|!=|<=|>=|[<>+\-*/=]', Operator),
            (r'\d+\.\d+', Number.Float),
            (r'\d+', Number.Integer),
            (r'[(),.]', Punctuation),
            (r'[a-zA-Z_]\w*', Name),
        ],
    }

    def analyse_text(text):
        # Scripts are delimited by ``begin <name>`` ... ``end``.
        if re.search(r'(?im)^\s*begin\s+\w', text) and \
                re.search(r'(?im)^\s*end\b', text):
            return 0.2
