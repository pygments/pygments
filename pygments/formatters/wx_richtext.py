import pygments.styles
from pygments.formatter import Formatter
from pygments.style import StyleMeta as PygmentsStyle
from pygments.token import _TokenType as Token


__all__ = ["WxRichTextCtrlFormatter"]


class WxRichTextCtrlFormatter(Formatter):
    """ 
    Formatter for applying a pygments style to a 
    ``wx.richtext.RichTextCtrl``. Each time `format` is 
    called, the ctrl's text is stored against its handle 
    so that subsequent calls to `format` won't style 
    unchanged text again. This means the function is fast 
    enough to call on each new character with minimal lag.

    Example:
    .. sourcecode:: python
    class MyStyledTextCtrl(wx.richtext.RichTextCtrl):
        def __init__(self, parent, style="default"):
            # initialise parent class
            wx.richtext.RichTextCtrl.__init__(
                self, parent
            )
            # create a lexer for this ctrl
            lexer = pygments.lexers.get_lexer_by_name("python")
            # create a formatter for this ctrl
            self.formatter = pygments.formatters.get_formatters_by_name("wx.rtc")
            # set formatter style
            self.formatter.set_style(style)
            # bind formatting call to new char
            self.Bind(wx.EVT_TEXT, self.on_text)
        
        def on_text(self, evt=None):
            # lex content for tokens
            tokens = pygments.lex(self.GetValue(), lexer=self.lexer)
            # format ctrl using tokens
            formatter.format(tokensource=tokens, outfile=self)  
    """

    name = "wxPython RichTextCtrl Formatter"
    aliases = ["wx", "wx.rtc", "wx.richtext"]

    def __init__(self, style="vs", **options):
        # initialise base class
        Formatter.__init__(self, style=style, **options)
        # make caches
        self._styles_cache = {}
        self._last_text_cache = {}
        # set style
        self.set_style(style)

    def get_style(self):
        """ 
        Get the current style used by this formatter
        """
        return self.style 

    def set_style(self, style):
        """ 
        Set the pygments style for this formatter to use.

        Parameters:

        `style`
            ``pygments.style.PygmentsStyle`` or ``str`` 
            pointing to a pygments style.
        """
        # get style object if given a valid name
        if style in pygments.styles.get_all_styles():
            style = pygments.styles.get_style_by_name(style)
        # make sure theme is a pygments style
        assert isinstance(style, PygmentsStyle), (
            "Expcted WxRichTextCtrlFormatter style to be a "
            "PygmentsStyle object or the name of a pygments "
            "style, but instead received {val} ({cls})"
        ).format(val=style, cls=type(style).__name__)
        # if style has changed, clear caches
        if style != self.style:
            self._styles_cache = {}
            self._last_text_cache = {}
        # store style object
        self.style = style

    def format(self, tokensource, outfile):
        """
        Takes an array of tokens and strings and applies them 
        to a ``wx.richtext.RichTextCtrl``, with built-in text 
        cacheing so that it can be called quickly with minimal 
        delay (e.g. when each new char is entered)

        Parameters:

        `tokensource`
            Iterable of ``(tokentype, tokenstring)`` tuples 
            (as outputted by lexers)

        `outfile`
            ``wx.richtext.RichTextCtrl`` object to apply 
            styling to
        """
        # locally import wx
        import wx.richtext

        # freeze while we style
        outfile.GetBuffer().BeginSuppressUndo()
        outfile.Freeze()
        
        # set global styling
        outfile.SetBackgroundColour(self.style.background_color)
        # set character style
        i = 0
        for token, text in tokensource:
            # get range
            rng = wx.richtext.RichTextRange(i, i + len(text))
            # move forward to next token
            i = rng.End
            # skip if token text has not changed...
            last_styled_text = self._last_text_cache.get(outfile, "")
            if (
                len(last_styled_text) > rng.End and 
                outfile.GetValue()[rng.Start:rng.End] == last_styled_text[rng.Start:rng.End]
            ):
                    continue
            # get formatting
            fmt = self.get_rtc_style(token, base=outfile.GetBasicStyle())
            # apply format object
            outfile.SetStyleEx(rng, fmt)
        # store text
        self._last_text_cache[outfile] = outfile.GetValue()
        
        # thaw once done
        outfile.GetBuffer().EndSuppressUndo()
        outfile.Thaw()
        outfile.Update()
        outfile.Refresh()
    
    def get_rtc_style(self, token:Token, base):
        """ 
        Get the ``wx.richtext.RichTextAttr`` associated with a 
        given token, using a fallback where styles aren't supplied.

        Style objects are cached once created so this should be 
        fast after the first call.

        Parameters:

        `token`
            Pygments token to get associated style for
        
        `base`
            ``wx.richtext.RichTextAttr`` whose styles to fallback on 
            if an atribute isn't specified by the given token's style
        """
        # import wx
        import wx.richtext

        # if cached, return cached value
        if token in self._styles_cache:
            return self._styles_cache[token]
        # get style spec for token
        spec = self.style.style_for_token(token)
        # make base font
        font = wx.richtext.RichTextAttr()
        font.Apply(base)
        # apply spec to font
        for key, family in (("roman", wx.FONTFAMILY_ROMAN), ("sans", wx.FONTFAMILY_SWISS), ("mono", wx.FONTFAMILY_TELETYPE)):
            if spec.get(key, None):
                font.SetFontFamily(family)
                break
        font.SetFontStyle(wx.FONTSTYLE_ITALIC if spec['italic'] else wx.FONTSTYLE_NORMAL)
        font.SetFontWeight(wx.FONTWEIGHT_BOLD if spec['bold'] else wx.FONTWEIGHT_NORMAL)
        font.SetFontUnderlined(wx.TEXT_ATTR_UNDERLINE_SOLID if spec['underline'] else wx.TEXT_ATTR_UNDERLINE_NONE)
        if spec['color'] is not None:
            font.SetTextColour(f"#{spec['color']}")
        # assign to styles dict
        self._styles_cache[token] = font

        return font