# -*- coding: utf-8 -*-
"""
    pygments.filters
    ~~~~~~~~~~~~~~~~

    Module containing filter lookup functions and default
    filters.

    :copyright: Copyright 2006-2020 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.token import String, Comment, Keyword, Name, Error, Whitespace, \
    string_to_tokentype
from pygments.filter import Filter
from pygments.util import get_list_opt, get_int_opt, get_bool_opt, \
    get_choice_opt, ClassNotFound, OptionError
from pygments.plugin import find_plugin_filters


def find_filter_class(filtername):
    """Lookup a filter by name. Return None if not found."""
    if filtername in FILTERS:
        return FILTERS[filtername]
    for name, cls in find_plugin_filters():
        if name == filtername:
            return cls
    return None


def get_filter_by_name(filtername, **options):
    """Return an instantiated filter.

    Options are passed to the filter initializer if wanted.
    Raise a ClassNotFound if not found.
    """
    cls = find_filter_class(filtername)
    if cls:
        return cls(**options)
    else:
        raise ClassNotFound('filter %r not found' % filtername)


def get_all_filters():
    """Return a generator of all filter names."""
    yield from FILTERS
    for name, _ in find_plugin_filters():
        yield name


def _replace_special(ttype, value, regex, specialttype,
                     replacefunc=lambda x: x):
    last = 0
    for match in regex.finditer(value):
        start, end = match.start(), match.end()
        if start != last:
            yield ttype, value[last:start]
        yield specialttype, replacefunc(value[start:end])
        last = end
    if last != len(value):
        yield ttype, value[last:]


class CodeTagFilter(Filter):
    """Highlight special code tags in comments and docstrings.

    Options accepted:

    `codetags` : list of strings
       A list of strings that are flagged as code tags.  The default is to
       highlight ``XXX``, ``TODO``, ``BUG`` and ``NOTE``.
    """

    def __init__(self, **options):
        Filter.__init__(self, **options)
        tags = get_list_opt(options, 'codetags',
                            ['XXX', 'TODO', 'BUG', 'NOTE'])
        self.tag_re = re.compile(r'\b(%s)\b' % '|'.join([
            re.escape(tag) for tag in tags if tag
        ]))

    def filter(self, lexer, stream):
        regex = self.tag_re
        for ttype, value in stream:
            if ttype in String.Doc or \
               ttype in Comment and \
               ttype not in Comment.Preproc:
                yield from _replace_special(ttype, value, regex, Comment.Special)
            else:
                yield ttype, value


class SymbolFilter(Filter):
    """Convert mathematical symbols such as \\<longrightarrow> in Isabelle
    or \\longrightarrow in LaTeX into Unicode characters.

    This is mostly useful for HTML or console output when you want to
    approximate the source rendering you'd see in an IDE.

    Options accepted:

    `lang` : string
       The symbol language. Must be one of ``'isabelle'`` or
       ``'latex'``.  The default is ``'isabelle'``.
    """

    latex_symbols = {
        '\\alpha'                : u'\U000003b1',
        '\\beta'                 : u'\U000003b2',
        '\\gamma'                : u'\U000003b3',
        '\\delta'                : u'\U000003b4',
        '\\varepsilon'           : u'\U000003b5',
        '\\zeta'                 : u'\U000003b6',
        '\\eta'                  : u'\U000003b7',
        '\\vartheta'             : u'\U000003b8',
        '\\iota'                 : u'\U000003b9',
        '\\kappa'                : u'\U000003ba',
        '\\lambda'               : u'\U000003bb',
        '\\mu'                   : u'\U000003bc',
        '\\nu'                   : u'\U000003bd',
        '\\xi'                   : u'\U000003be',
        '\\pi'                   : u'\U000003c0',
        '\\varrho'               : u'\U000003c1',
        '\\sigma'                : u'\U000003c3',
        '\\tau'                  : u'\U000003c4',
        '\\upsilon'              : u'\U000003c5',
        '\\varphi'               : u'\U000003c6',
        '\\chi'                  : u'\U000003c7',
        '\\psi'                  : u'\U000003c8',
        '\\omega'                : u'\U000003c9',
        '\\Gamma'                : u'\U00000393',
        '\\Delta'                : u'\U00000394',
        '\\Theta'                : u'\U00000398',
        '\\Lambda'               : u'\U0000039b',
        '\\Xi'                   : u'\U0000039e',
        '\\Pi'                   : u'\U000003a0',
        '\\Sigma'                : u'\U000003a3',
        '\\Upsilon'              : u'\U000003a5',
        '\\Phi'                  : u'\U000003a6',
        '\\Psi'                  : u'\U000003a8',
        '\\Omega'                : u'\U000003a9',
        '\\leftarrow'            : u'\U00002190',
        '\\longleftarrow'        : u'\U000027f5',
        '\\rightarrow'           : u'\U00002192',
        '\\longrightarrow'       : u'\U000027f6',
        '\\Leftarrow'            : u'\U000021d0',
        '\\Longleftarrow'        : u'\U000027f8',
        '\\Rightarrow'           : u'\U000021d2',
        '\\Longrightarrow'       : u'\U000027f9',
        '\\leftrightarrow'       : u'\U00002194',
        '\\longleftrightarrow'   : u'\U000027f7',
        '\\Leftrightarrow'       : u'\U000021d4',
        '\\Longleftrightarrow'   : u'\U000027fa',
        '\\mapsto'               : u'\U000021a6',
        '\\longmapsto'           : u'\U000027fc',
        '\\relbar'               : u'\U00002500',
        '\\Relbar'               : u'\U00002550',
        '\\hookleftarrow'        : u'\U000021a9',
        '\\hookrightarrow'       : u'\U000021aa',
        '\\leftharpoondown'      : u'\U000021bd',
        '\\rightharpoondown'     : u'\U000021c1',
        '\\leftharpoonup'        : u'\U000021bc',
        '\\rightharpoonup'       : u'\U000021c0',
        '\\rightleftharpoons'    : u'\U000021cc',
        '\\leadsto'              : u'\U0000219d',
        '\\downharpoonleft'      : u'\U000021c3',
        '\\downharpoonright'     : u'\U000021c2',
        '\\upharpoonleft'        : u'\U000021bf',
        '\\upharpoonright'       : u'\U000021be',
        '\\restriction'          : u'\U000021be',
        '\\uparrow'              : u'\U00002191',
        '\\Uparrow'              : u'\U000021d1',
        '\\downarrow'            : u'\U00002193',
        '\\Downarrow'            : u'\U000021d3',
        '\\updownarrow'          : u'\U00002195',
        '\\Updownarrow'          : u'\U000021d5',
        '\\langle'               : u'\U000027e8',
        '\\rangle'               : u'\U000027e9',
        '\\lceil'                : u'\U00002308',
        '\\rceil'                : u'\U00002309',
        '\\lfloor'               : u'\U0000230a',
        '\\rfloor'               : u'\U0000230b',
        '\\flqq'                 : u'\U000000ab',
        '\\frqq'                 : u'\U000000bb',
        '\\bot'                  : u'\U000022a5',
        '\\top'                  : u'\U000022a4',
        '\\wedge'                : u'\U00002227',
        '\\bigwedge'             : u'\U000022c0',
        '\\vee'                  : u'\U00002228',
        '\\bigvee'               : u'\U000022c1',
        '\\forall'               : u'\U00002200',
        '\\exists'               : u'\U00002203',
        '\\nexists'              : u'\U00002204',
        '\\neg'                  : u'\U000000ac',
        '\\Box'                  : u'\U000025a1',
        '\\Diamond'              : u'\U000025c7',
        '\\vdash'                : u'\U000022a2',
        '\\models'               : u'\U000022a8',
        '\\dashv'                : u'\U000022a3',
        '\\surd'                 : u'\U0000221a',
        '\\le'                   : u'\U00002264',
        '\\ge'                   : u'\U00002265',
        '\\ll'                   : u'\U0000226a',
        '\\gg'                   : u'\U0000226b',
        '\\lesssim'              : u'\U00002272',
        '\\gtrsim'               : u'\U00002273',
        '\\lessapprox'           : u'\U00002a85',
        '\\gtrapprox'            : u'\U00002a86',
        '\\in'                   : u'\U00002208',
        '\\notin'                : u'\U00002209',
        '\\subset'               : u'\U00002282',
        '\\supset'               : u'\U00002283',
        '\\subseteq'             : u'\U00002286',
        '\\supseteq'             : u'\U00002287',
        '\\sqsubset'             : u'\U0000228f',
        '\\sqsupset'             : u'\U00002290',
        '\\sqsubseteq'           : u'\U00002291',
        '\\sqsupseteq'           : u'\U00002292',
        '\\cap'                  : u'\U00002229',
        '\\bigcap'               : u'\U000022c2',
        '\\cup'                  : u'\U0000222a',
        '\\bigcup'               : u'\U000022c3',
        '\\sqcup'                : u'\U00002294',
        '\\bigsqcup'             : u'\U00002a06',
        '\\sqcap'                : u'\U00002293',
        '\\Bigsqcap'             : u'\U00002a05',
        '\\setminus'             : u'\U00002216',
        '\\propto'               : u'\U0000221d',
        '\\uplus'                : u'\U0000228e',
        '\\bigplus'              : u'\U00002a04',
        '\\sim'                  : u'\U0000223c',
        '\\doteq'                : u'\U00002250',
        '\\simeq'                : u'\U00002243',
        '\\approx'               : u'\U00002248',
        '\\asymp'                : u'\U0000224d',
        '\\cong'                 : u'\U00002245',
        '\\equiv'                : u'\U00002261',
        '\\Join'                 : u'\U000022c8',
        '\\bowtie'               : u'\U00002a1d',
        '\\prec'                 : u'\U0000227a',
        '\\succ'                 : u'\U0000227b',
        '\\preceq'               : u'\U0000227c',
        '\\succeq'               : u'\U0000227d',
        '\\parallel'             : u'\U00002225',
        '\\mid'                  : u'\U000000a6',
        '\\pm'                   : u'\U000000b1',
        '\\mp'                   : u'\U00002213',
        '\\times'                : u'\U000000d7',
        '\\div'                  : u'\U000000f7',
        '\\cdot'                 : u'\U000022c5',
        '\\star'                 : u'\U000022c6',
        '\\circ'                 : u'\U00002218',
        '\\dagger'               : u'\U00002020',
        '\\ddagger'              : u'\U00002021',
        '\\lhd'                  : u'\U000022b2',
        '\\rhd'                  : u'\U000022b3',
        '\\unlhd'                : u'\U000022b4',
        '\\unrhd'                : u'\U000022b5',
        '\\triangleleft'         : u'\U000025c3',
        '\\triangleright'        : u'\U000025b9',
        '\\triangle'             : u'\U000025b3',
        '\\triangleq'            : u'\U0000225c',
        '\\oplus'                : u'\U00002295',
        '\\bigoplus'             : u'\U00002a01',
        '\\otimes'               : u'\U00002297',
        '\\bigotimes'            : u'\U00002a02',
        '\\odot'                 : u'\U00002299',
        '\\bigodot'              : u'\U00002a00',
        '\\ominus'               : u'\U00002296',
        '\\oslash'               : u'\U00002298',
        '\\dots'                 : u'\U00002026',
        '\\cdots'                : u'\U000022ef',
        '\\sum'                  : u'\U00002211',
        '\\prod'                 : u'\U0000220f',
        '\\coprod'               : u'\U00002210',
        '\\infty'                : u'\U0000221e',
        '\\int'                  : u'\U0000222b',
        '\\oint'                 : u'\U0000222e',
        '\\clubsuit'             : u'\U00002663',
        '\\diamondsuit'          : u'\U00002662',
        '\\heartsuit'            : u'\U00002661',
        '\\spadesuit'            : u'\U00002660',
        '\\aleph'                : u'\U00002135',
        '\\emptyset'             : u'\U00002205',
        '\\nabla'                : u'\U00002207',
        '\\partial'              : u'\U00002202',
        '\\flat'                 : u'\U0000266d',
        '\\natural'              : u'\U0000266e',
        '\\sharp'                : u'\U0000266f',
        '\\angle'                : u'\U00002220',
        '\\copyright'            : u'\U000000a9',
        '\\textregistered'       : u'\U000000ae',
        '\\textonequarter'       : u'\U000000bc',
        '\\textonehalf'          : u'\U000000bd',
        '\\textthreequarters'    : u'\U000000be',
        '\\textordfeminine'      : u'\U000000aa',
        '\\textordmasculine'     : u'\U000000ba',
        '\\euro'                 : u'\U000020ac',
        '\\pounds'               : u'\U000000a3',
        '\\yen'                  : u'\U000000a5',
        '\\textcent'             : u'\U000000a2',
        '\\textcurrency'         : u'\U000000a4',
        '\\textdegree'           : u'\U000000b0',
    }

    isabelle_symbols = {
        '\\<zero>'                 : u'\U0001d7ec',
        '\\<one>'                  : u'\U0001d7ed',
        '\\<two>'                  : u'\U0001d7ee',
        '\\<three>'                : u'\U0001d7ef',
        '\\<four>'                 : u'\U0001d7f0',
        '\\<five>'                 : u'\U0001d7f1',
        '\\<six>'                  : u'\U0001d7f2',
        '\\<seven>'                : u'\U0001d7f3',
        '\\<eight>'                : u'\U0001d7f4',
        '\\<nine>'                 : u'\U0001d7f5',
        '\\<A>'                    : u'\U0001d49c',
        '\\<B>'                    : u'\U0000212c',
        '\\<C>'                    : u'\U0001d49e',
        '\\<D>'                    : u'\U0001d49f',
        '\\<E>'                    : u'\U00002130',
        '\\<F>'                    : u'\U00002131',
        '\\<G>'                    : u'\U0001d4a2',
        '\\<H>'                    : u'\U0000210b',
        '\\<I>'                    : u'\U00002110',
        '\\<J>'                    : u'\U0001d4a5',
        '\\<K>'                    : u'\U0001d4a6',
        '\\<L>'                    : u'\U00002112',
        '\\<M>'                    : u'\U00002133',
        '\\<N>'                    : u'\U0001d4a9',
        '\\<O>'                    : u'\U0001d4aa',
        '\\<P>'                    : u'\U0001d4ab',
        '\\<Q>'                    : u'\U0001d4ac',
        '\\<R>'                    : u'\U0000211b',
        '\\<S>'                    : u'\U0001d4ae',
        '\\<T>'                    : u'\U0001d4af',
        '\\<U>'                    : u'\U0001d4b0',
        '\\<V>'                    : u'\U0001d4b1',
        '\\<W>'                    : u'\U0001d4b2',
        '\\<X>'                    : u'\U0001d4b3',
        '\\<Y>'                    : u'\U0001d4b4',
        '\\<Z>'                    : u'\U0001d4b5',
        '\\<a>'                    : u'\U0001d5ba',
        '\\<b>'                    : u'\U0001d5bb',
        '\\<c>'                    : u'\U0001d5bc',
        '\\<d>'                    : u'\U0001d5bd',
        '\\<e>'                    : u'\U0001d5be',
        '\\<f>'                    : u'\U0001d5bf',
        '\\<g>'                    : u'\U0001d5c0',
        '\\<h>'                    : u'\U0001d5c1',
        '\\<i>'                    : u'\U0001d5c2',
        '\\<j>'                    : u'\U0001d5c3',
        '\\<k>'                    : u'\U0001d5c4',
        '\\<l>'                    : u'\U0001d5c5',
        '\\<m>'                    : u'\U0001d5c6',
        '\\<n>'                    : u'\U0001d5c7',
        '\\<o>'                    : u'\U0001d5c8',
        '\\<p>'                    : u'\U0001d5c9',
        '\\<q>'                    : u'\U0001d5ca',
        '\\<r>'                    : u'\U0001d5cb',
        '\\<s>'                    : u'\U0001d5cc',
        '\\<t>'                    : u'\U0001d5cd',
        '\\<u>'                    : u'\U0001d5ce',
        '\\<v>'                    : u'\U0001d5cf',
        '\\<w>'                    : u'\U0001d5d0',
        '\\<x>'                    : u'\U0001d5d1',
        '\\<y>'                    : u'\U0001d5d2',
        '\\<z>'                    : u'\U0001d5d3',
        '\\<AA>'                   : u'\U0001d504',
        '\\<BB>'                   : u'\U0001d505',
        '\\<CC>'                   : u'\U0000212d',
        '\\<DD>'                   : u'\U0001d507',
        '\\<EE>'                   : u'\U0001d508',
        '\\<FF>'                   : u'\U0001d509',
        '\\<GG>'                   : u'\U0001d50a',
        '\\<HH>'                   : u'\U0000210c',
        '\\<II>'                   : u'\U00002111',
        '\\<JJ>'                   : u'\U0001d50d',
        '\\<KK>'                   : u'\U0001d50e',
        '\\<LL>'                   : u'\U0001d50f',
        '\\<MM>'                   : u'\U0001d510',
        '\\<NN>'                   : u'\U0001d511',
        '\\<OO>'                   : u'\U0001d512',
        '\\<PP>'                   : u'\U0001d513',
        '\\<QQ>'                   : u'\U0001d514',
        '\\<RR>'                   : u'\U0000211c',
        '\\<SS>'                   : u'\U0001d516',
        '\\<TT>'                   : u'\U0001d517',
        '\\<UU>'                   : u'\U0001d518',
        '\\<VV>'                   : u'\U0001d519',
        '\\<WW>'                   : u'\U0001d51a',
        '\\<XX>'                   : u'\U0001d51b',
        '\\<YY>'                   : u'\U0001d51c',
        '\\<ZZ>'                   : u'\U00002128',
        '\\<aa>'                   : u'\U0001d51e',
        '\\<bb>'                   : u'\U0001d51f',
        '\\<cc>'                   : u'\U0001d520',
        '\\<dd>'                   : u'\U0001d521',
        '\\<ee>'                   : u'\U0001d522',
        '\\<ff>'                   : u'\U0001d523',
        '\\<gg>'                   : u'\U0001d524',
        '\\<hh>'                   : u'\U0001d525',
        '\\<ii>'                   : u'\U0001d526',
        '\\<jj>'                   : u'\U0001d527',
        '\\<kk>'                   : u'\U0001d528',
        '\\<ll>'                   : u'\U0001d529',
        '\\<mm>'                   : u'\U0001d52a',
        '\\<nn>'                   : u'\U0001d52b',
        '\\<oo>'                   : u'\U0001d52c',
        '\\<pp>'                   : u'\U0001d52d',
        '\\<qq>'                   : u'\U0001d52e',
        '\\<rr>'                   : u'\U0001d52f',
        '\\<ss>'                   : u'\U0001d530',
        '\\<tt>'                   : u'\U0001d531',
        '\\<uu>'                   : u'\U0001d532',
        '\\<vv>'                   : u'\U0001d533',
        '\\<ww>'                   : u'\U0001d534',
        '\\<xx>'                   : u'\U0001d535',
        '\\<yy>'                   : u'\U0001d536',
        '\\<zz>'                   : u'\U0001d537',
        '\\<alpha>'                : u'\U000003b1',
        '\\<beta>'                 : u'\U000003b2',
        '\\<gamma>'                : u'\U000003b3',
        '\\<delta>'                : u'\U000003b4',
        '\\<epsilon>'              : u'\U000003b5',
        '\\<zeta>'                 : u'\U000003b6',
        '\\<eta>'                  : u'\U000003b7',
        '\\<theta>'                : u'\U000003b8',
        '\\<iota>'                 : u'\U000003b9',
        '\\<kappa>'                : u'\U000003ba',
        '\\<lambda>'               : u'\U000003bb',
        '\\<mu>'                   : u'\U000003bc',
        '\\<nu>'                   : u'\U000003bd',
        '\\<xi>'                   : u'\U000003be',
        '\\<pi>'                   : u'\U000003c0',
        '\\<rho>'                  : u'\U000003c1',
        '\\<sigma>'                : u'\U000003c3',
        '\\<tau>'                  : u'\U000003c4',
        '\\<upsilon>'              : u'\U000003c5',
        '\\<phi>'                  : u'\U000003c6',
        '\\<chi>'                  : u'\U000003c7',
        '\\<psi>'                  : u'\U000003c8',
        '\\<omega>'                : u'\U000003c9',
        '\\<Gamma>'                : u'\U00000393',
        '\\<Delta>'                : u'\U00000394',
        '\\<Theta>'                : u'\U00000398',
        '\\<Lambda>'               : u'\U0000039b',
        '\\<Xi>'                   : u'\U0000039e',
        '\\<Pi>'                   : u'\U000003a0',
        '\\<Sigma>'                : u'\U000003a3',
        '\\<Upsilon>'              : u'\U000003a5',
        '\\<Phi>'                  : u'\U000003a6',
        '\\<Psi>'                  : u'\U000003a8',
        '\\<Omega>'                : u'\U000003a9',
        '\\<bool>'                 : u'\U0001d539',
        '\\<complex>'              : u'\U00002102',
        '\\<nat>'                  : u'\U00002115',
        '\\<rat>'                  : u'\U0000211a',
        '\\<real>'                 : u'\U0000211d',
        '\\<int>'                  : u'\U00002124',
        '\\<leftarrow>'            : u'\U00002190',
        '\\<longleftarrow>'        : u'\U000027f5',
        '\\<rightarrow>'           : u'\U00002192',
        '\\<longrightarrow>'       : u'\U000027f6',
        '\\<Leftarrow>'            : u'\U000021d0',
        '\\<Longleftarrow>'        : u'\U000027f8',
        '\\<Rightarrow>'           : u'\U000021d2',
        '\\<Longrightarrow>'       : u'\U000027f9',
        '\\<leftrightarrow>'       : u'\U00002194',
        '\\<longleftrightarrow>'   : u'\U000027f7',
        '\\<Leftrightarrow>'       : u'\U000021d4',
        '\\<Longleftrightarrow>'   : u'\U000027fa',
        '\\<mapsto>'               : u'\U000021a6',
        '\\<longmapsto>'           : u'\U000027fc',
        '\\<midarrow>'             : u'\U00002500',
        '\\<Midarrow>'             : u'\U00002550',
        '\\<hookleftarrow>'        : u'\U000021a9',
        '\\<hookrightarrow>'       : u'\U000021aa',
        '\\<leftharpoondown>'      : u'\U000021bd',
        '\\<rightharpoondown>'     : u'\U000021c1',
        '\\<leftharpoonup>'        : u'\U000021bc',
        '\\<rightharpoonup>'       : u'\U000021c0',
        '\\<rightleftharpoons>'    : u'\U000021cc',
        '\\<leadsto>'              : u'\U0000219d',
        '\\<downharpoonleft>'      : u'\U000021c3',
        '\\<downharpoonright>'     : u'\U000021c2',
        '\\<upharpoonleft>'        : u'\U000021bf',
        '\\<upharpoonright>'       : u'\U000021be',
        '\\<restriction>'          : u'\U000021be',
        '\\<Colon>'                : u'\U00002237',
        '\\<up>'                   : u'\U00002191',
        '\\<Up>'                   : u'\U000021d1',
        '\\<down>'                 : u'\U00002193',
        '\\<Down>'                 : u'\U000021d3',
        '\\<updown>'               : u'\U00002195',
        '\\<Updown>'               : u'\U000021d5',
        '\\<langle>'               : u'\U000027e8',
        '\\<rangle>'               : u'\U000027e9',
        '\\<lceil>'                : u'\U00002308',
        '\\<rceil>'                : u'\U00002309',
        '\\<lfloor>'               : u'\U0000230a',
        '\\<rfloor>'               : u'\U0000230b',
        '\\<lparr>'                : u'\U00002987',
        '\\<rparr>'                : u'\U00002988',
        '\\<lbrakk>'               : u'\U000027e6',
        '\\<rbrakk>'               : u'\U000027e7',
        '\\<lbrace>'               : u'\U00002983',
        '\\<rbrace>'               : u'\U00002984',
        '\\<guillemotleft>'        : u'\U000000ab',
        '\\<guillemotright>'       : u'\U000000bb',
        '\\<bottom>'               : u'\U000022a5',
        '\\<top>'                  : u'\U000022a4',
        '\\<and>'                  : u'\U00002227',
        '\\<And>'                  : u'\U000022c0',
        '\\<or>'                   : u'\U00002228',
        '\\<Or>'                   : u'\U000022c1',
        '\\<forall>'               : u'\U00002200',
        '\\<exists>'               : u'\U00002203',
        '\\<nexists>'              : u'\U00002204',
        '\\<not>'                  : u'\U000000ac',
        '\\<box>'                  : u'\U000025a1',
        '\\<diamond>'              : u'\U000025c7',
        '\\<turnstile>'            : u'\U000022a2',
        '\\<Turnstile>'            : u'\U000022a8',
        '\\<tturnstile>'           : u'\U000022a9',
        '\\<TTurnstile>'           : u'\U000022ab',
        '\\<stileturn>'            : u'\U000022a3',
        '\\<surd>'                 : u'\U0000221a',
        '\\<le>'                   : u'\U00002264',
        '\\<ge>'                   : u'\U00002265',
        '\\<lless>'                : u'\U0000226a',
        '\\<ggreater>'             : u'\U0000226b',
        '\\<lesssim>'              : u'\U00002272',
        '\\<greatersim>'           : u'\U00002273',
        '\\<lessapprox>'           : u'\U00002a85',
        '\\<greaterapprox>'        : u'\U00002a86',
        '\\<in>'                   : u'\U00002208',
        '\\<notin>'                : u'\U00002209',
        '\\<subset>'               : u'\U00002282',
        '\\<supset>'               : u'\U00002283',
        '\\<subseteq>'             : u'\U00002286',
        '\\<supseteq>'             : u'\U00002287',
        '\\<sqsubset>'             : u'\U0000228f',
        '\\<sqsupset>'             : u'\U00002290',
        '\\<sqsubseteq>'           : u'\U00002291',
        '\\<sqsupseteq>'           : u'\U00002292',
        '\\<inter>'                : u'\U00002229',
        '\\<Inter>'                : u'\U000022c2',
        '\\<union>'                : u'\U0000222a',
        '\\<Union>'                : u'\U000022c3',
        '\\<squnion>'              : u'\U00002294',
        '\\<Squnion>'              : u'\U00002a06',
        '\\<sqinter>'              : u'\U00002293',
        '\\<Sqinter>'              : u'\U00002a05',
        '\\<setminus>'             : u'\U00002216',
        '\\<propto>'               : u'\U0000221d',
        '\\<uplus>'                : u'\U0000228e',
        '\\<Uplus>'                : u'\U00002a04',
        '\\<noteq>'                : u'\U00002260',
        '\\<sim>'                  : u'\U0000223c',
        '\\<doteq>'                : u'\U00002250',
        '\\<simeq>'                : u'\U00002243',
        '\\<approx>'               : u'\U00002248',
        '\\<asymp>'                : u'\U0000224d',
        '\\<cong>'                 : u'\U00002245',
        '\\<smile>'                : u'\U00002323',
        '\\<equiv>'                : u'\U00002261',
        '\\<frown>'                : u'\U00002322',
        '\\<Join>'                 : u'\U000022c8',
        '\\<bowtie>'               : u'\U00002a1d',
        '\\<prec>'                 : u'\U0000227a',
        '\\<succ>'                 : u'\U0000227b',
        '\\<preceq>'               : u'\U0000227c',
        '\\<succeq>'               : u'\U0000227d',
        '\\<parallel>'             : u'\U00002225',
        '\\<bar>'                  : u'\U000000a6',
        '\\<plusminus>'            : u'\U000000b1',
        '\\<minusplus>'            : u'\U00002213',
        '\\<times>'                : u'\U000000d7',
        '\\<div>'                  : u'\U000000f7',
        '\\<cdot>'                 : u'\U000022c5',
        '\\<star>'                 : u'\U000022c6',
        '\\<bullet>'               : u'\U00002219',
        '\\<circ>'                 : u'\U00002218',
        '\\<dagger>'               : u'\U00002020',
        '\\<ddagger>'              : u'\U00002021',
        '\\<lhd>'                  : u'\U000022b2',
        '\\<rhd>'                  : u'\U000022b3',
        '\\<unlhd>'                : u'\U000022b4',
        '\\<unrhd>'                : u'\U000022b5',
        '\\<triangleleft>'         : u'\U000025c3',
        '\\<triangleright>'        : u'\U000025b9',
        '\\<triangle>'             : u'\U000025b3',
        '\\<triangleq>'            : u'\U0000225c',
        '\\<oplus>'                : u'\U00002295',
        '\\<Oplus>'                : u'\U00002a01',
        '\\<otimes>'               : u'\U00002297',
        '\\<Otimes>'               : u'\U00002a02',
        '\\<odot>'                 : u'\U00002299',
        '\\<Odot>'                 : u'\U00002a00',
        '\\<ominus>'               : u'\U00002296',
        '\\<oslash>'               : u'\U00002298',
        '\\<dots>'                 : u'\U00002026',
        '\\<cdots>'                : u'\U000022ef',
        '\\<Sum>'                  : u'\U00002211',
        '\\<Prod>'                 : u'\U0000220f',
        '\\<Coprod>'               : u'\U00002210',
        '\\<infinity>'             : u'\U0000221e',
        '\\<integral>'             : u'\U0000222b',
        '\\<ointegral>'            : u'\U0000222e',
        '\\<clubsuit>'             : u'\U00002663',
        '\\<diamondsuit>'          : u'\U00002662',
        '\\<heartsuit>'            : u'\U00002661',
        '\\<spadesuit>'            : u'\U00002660',
        '\\<aleph>'                : u'\U00002135',
        '\\<emptyset>'             : u'\U00002205',
        '\\<nabla>'                : u'\U00002207',
        '\\<partial>'              : u'\U00002202',
        '\\<flat>'                 : u'\U0000266d',
        '\\<natural>'              : u'\U0000266e',
        '\\<sharp>'                : u'\U0000266f',
        '\\<angle>'                : u'\U00002220',
        '\\<copyright>'            : u'\U000000a9',
        '\\<registered>'           : u'\U000000ae',
        '\\<hyphen>'               : u'\U000000ad',
        '\\<inverse>'              : u'\U000000af',
        '\\<onequarter>'           : u'\U000000bc',
        '\\<onehalf>'              : u'\U000000bd',
        '\\<threequarters>'        : u'\U000000be',
        '\\<ordfeminine>'          : u'\U000000aa',
        '\\<ordmasculine>'         : u'\U000000ba',
        '\\<section>'              : u'\U000000a7',
        '\\<paragraph>'            : u'\U000000b6',
        '\\<exclamdown>'           : u'\U000000a1',
        '\\<questiondown>'         : u'\U000000bf',
        '\\<euro>'                 : u'\U000020ac',
        '\\<pounds>'               : u'\U000000a3',
        '\\<yen>'                  : u'\U000000a5',
        '\\<cent>'                 : u'\U000000a2',
        '\\<currency>'             : u'\U000000a4',
        '\\<degree>'               : u'\U000000b0',
        '\\<amalg>'                : u'\U00002a3f',
        '\\<mho>'                  : u'\U00002127',
        '\\<lozenge>'              : u'\U000025ca',
        '\\<wp>'                   : u'\U00002118',
        '\\<wrong>'                : u'\U00002240',
        '\\<struct>'               : u'\U000022c4',
        '\\<acute>'                : u'\U000000b4',
        '\\<index>'                : u'\U00000131',
        '\\<dieresis>'             : u'\U000000a8',
        '\\<cedilla>'              : u'\U000000b8',
        '\\<hungarumlaut>'         : u'\U000002dd',
        '\\<some>'                 : u'\U000003f5',
        '\\<newline>'              : u'\U000023ce',
        '\\<open>'                 : u'\U00002039',
        '\\<close>'                : u'\U0000203a',
        '\\<here>'                 : u'\U00002302',
        '\\<^sub>'                 : u'\U000021e9',
        '\\<^sup>'                 : u'\U000021e7',
        '\\<^bold>'                : u'\U00002759',
        '\\<^bsub>'                : u'\U000021d8',
        '\\<^esub>'                : u'\U000021d9',
        '\\<^bsup>'                : u'\U000021d7',
        '\\<^esup>'                : u'\U000021d6',
    }

    lang_map = {'isabelle' : isabelle_symbols, 'latex' : latex_symbols}

    def __init__(self, **options):
        Filter.__init__(self, **options)
        lang = get_choice_opt(options, 'lang',
                              ['isabelle', 'latex'], 'isabelle')
        self.symbols = self.lang_map[lang]

    def filter(self, lexer, stream):
        for ttype, value in stream:
            if value in self.symbols:
                yield ttype, self.symbols[value]
            else:
                yield ttype, value


class KeywordCaseFilter(Filter):
    """Convert keywords to lowercase or uppercase or capitalize them, which
    means first letter uppercase, rest lowercase.

    This can be useful e.g. if you highlight Pascal code and want to adapt the
    code to your styleguide.

    Options accepted:

    `case` : string
       The casing to convert keywords to. Must be one of ``'lower'``,
       ``'upper'`` or ``'capitalize'``.  The default is ``'lower'``.
    """

    def __init__(self, **options):
        Filter.__init__(self, **options)
        case = get_choice_opt(options, 'case',
                              ['lower', 'upper', 'capitalize'], 'lower')
        self.convert = getattr(str, case)

    def filter(self, lexer, stream):
        for ttype, value in stream:
            if ttype in Keyword:
                yield ttype, self.convert(value)
            else:
                yield ttype, value


class NameHighlightFilter(Filter):
    """Highlight a normal Name (and Name.*) token with a different token type.

    Example::

        filter = NameHighlightFilter(
            names=['foo', 'bar', 'baz'],
            tokentype=Name.Function,
        )

    This would highlight the names "foo", "bar" and "baz"
    as functions. `Name.Function` is the default token type.

    Options accepted:

    `names` : list of strings
      A list of names that should be given the different token type.
      There is no default.
    `tokentype` : TokenType or string
      A token type or a string containing a token type name that is
      used for highlighting the strings in `names`.  The default is
      `Name.Function`.
    """

    def __init__(self, **options):
        Filter.__init__(self, **options)
        self.names = set(get_list_opt(options, 'names', []))
        tokentype = options.get('tokentype')
        if tokentype:
            self.tokentype = string_to_tokentype(tokentype)
        else:
            self.tokentype = Name.Function

    def filter(self, lexer, stream):
        for ttype, value in stream:
            if ttype in Name and value in self.names:
                yield self.tokentype, value
            else:
                yield ttype, value


class ErrorToken(Exception):
    pass


class RaiseOnErrorTokenFilter(Filter):
    """Raise an exception when the lexer generates an error token.

    Options accepted:

    `excclass` : Exception class
      The exception class to raise.
      The default is `pygments.filters.ErrorToken`.

    .. versionadded:: 0.8
    """

    def __init__(self, **options):
        Filter.__init__(self, **options)
        self.exception = options.get('excclass', ErrorToken)
        try:
            # issubclass() will raise TypeError if first argument is not a class
            if not issubclass(self.exception, Exception):
                raise TypeError
        except TypeError:
            raise OptionError('excclass option is not an exception class')

    def filter(self, lexer, stream):
        for ttype, value in stream:
            if ttype is Error:
                raise self.exception(value)
            yield ttype, value


class VisibleWhitespaceFilter(Filter):
    """Convert tabs, newlines and/or spaces to visible characters.

    Options accepted:

    `spaces` : string or bool
      If this is a one-character string, spaces will be replaces by this string.
      If it is another true value, spaces will be replaced by ``·`` (unicode
      MIDDLE DOT).  If it is a false value, spaces will not be replaced.  The
      default is ``False``.
    `tabs` : string or bool
      The same as for `spaces`, but the default replacement character is ``»``
      (unicode RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK).  The default value
      is ``False``.  Note: this will not work if the `tabsize` option for the
      lexer is nonzero, as tabs will already have been expanded then.
    `tabsize` : int
      If tabs are to be replaced by this filter (see the `tabs` option), this
      is the total number of characters that a tab should be expanded to.
      The default is ``8``.
    `newlines` : string or bool
      The same as for `spaces`, but the default replacement character is ``¶``
      (unicode PILCROW SIGN).  The default value is ``False``.
    `wstokentype` : bool
      If true, give whitespace the special `Whitespace` token type.  This allows
      styling the visible whitespace differently (e.g. greyed out), but it can
      disrupt background colors.  The default is ``True``.

    .. versionadded:: 0.8
    """

    def __init__(self, **options):
        Filter.__init__(self, **options)
        for name, default in [('spaces',   u'·'),
                              ('tabs',     u'»'),
                              ('newlines', u'¶')]:
            opt = options.get(name, False)
            if isinstance(opt, str) and len(opt) == 1:
                setattr(self, name, opt)
            else:
                setattr(self, name, (opt and default or ''))
        tabsize = get_int_opt(options, 'tabsize', 8)
        if self.tabs:
            self.tabs += ' ' * (tabsize - 1)
        if self.newlines:
            self.newlines += '\n'
        self.wstt = get_bool_opt(options, 'wstokentype', True)

    def filter(self, lexer, stream):
        if self.wstt:
            spaces = self.spaces or u' '
            tabs = self.tabs or u'\t'
            newlines = self.newlines or u'\n'
            regex = re.compile(r'\s')

            def replacefunc(wschar):
                if wschar == ' ':
                    return spaces
                elif wschar == '\t':
                    return tabs
                elif wschar == '\n':
                    return newlines
                return wschar

            for ttype, value in stream:
                yield from _replace_special(ttype, value, regex, Whitespace,
                                            replacefunc)
        else:
            spaces, tabs, newlines = self.spaces, self.tabs, self.newlines
            # simpler processing
            for ttype, value in stream:
                if spaces:
                    value = value.replace(' ', spaces)
                if tabs:
                    value = value.replace('\t', tabs)
                if newlines:
                    value = value.replace('\n', newlines)
                yield ttype, value


class GobbleFilter(Filter):
    """Gobbles source code lines (eats initial characters).

    This filter drops the first ``n`` characters off every line of code.  This
    may be useful when the source code fed to the lexer is indented by a fixed
    amount of space that isn't desired in the output.

    Options accepted:

    `n` : int
       The number of characters to gobble.

    .. versionadded:: 1.2
    """
    def __init__(self, **options):
        Filter.__init__(self, **options)
        self.n = get_int_opt(options, 'n', 0)

    def gobble(self, value, left):
        if left < len(value):
            return value[left:], 0
        else:
            return u'', left - len(value)

    def filter(self, lexer, stream):
        n = self.n
        left = n  # How many characters left to gobble.
        for ttype, value in stream:
            # Remove ``left`` tokens from first line, ``n`` from all others.
            parts = value.split('\n')
            (parts[0], left) = self.gobble(parts[0], left)
            for i in range(1, len(parts)):
                (parts[i], left) = self.gobble(parts[i], n)
            value = u'\n'.join(parts)

            if value != '':
                yield ttype, value


class TokenMergeFilter(Filter):
    """Merges consecutive tokens with the same token type in the output
    stream of a lexer.

    .. versionadded:: 1.2
    """
    def __init__(self, **options):
        Filter.__init__(self, **options)

    def filter(self, lexer, stream):
        current_type = None
        current_value = None
        for ttype, value in stream:
            if ttype is current_type:
                current_value += value
            else:
                if current_type is not None:
                    yield current_type, current_value
                current_type = ttype
                current_value = value
        if current_type is not None:
            yield current_type, current_value


FILTERS = {
    'codetagify':     CodeTagFilter,
    'keywordcase':    KeywordCaseFilter,
    'highlight':      NameHighlightFilter,
    'raiseonerror':   RaiseOnErrorTokenFilter,
    'whitespace':     VisibleWhitespaceFilter,
    'gobble':         GobbleFilter,
    'tokenmerge':     TokenMergeFilter,
    'symbols':        SymbolFilter,
}
