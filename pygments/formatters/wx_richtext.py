import pygments.styles
from pygments.formatter import Formatter
from pygments.style import StyleMeta as PygmentsStyle
from pygments.token import _TokenType as Token


__all__ = ["WxRichTextCtrlFormatter"]


class WxRichTextCtrlFormatter(Formatter):
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
        
        return self.style 

    def set_style(self, style):
        # get style object if given a valid name
        if style in pygments.styles.get_all_styles():
            style = pygments.styles.get_style_by_name(style)
        # make sure theme is a pygments style
        assert isinstance(style, PygmentsStyle), (
            "Expcted PygmentsStyledTextCtrl theme to be a PygmentsStyle object or the name of a pygments style, but instead received {val} ({cls})"
        ).format(val=style, cls=type(style).__name__)
        # if style has changed, clear caches
        if style != self.style:
            self._styles_cache = {}
            self._last_text_cache = {}
        # store style object
        self.style = style

    def format(self, tokensource, outfile):
        # locally import wx
        import pygments.formatters.wx_richtext

        # freeze while we style
        outfile.GetBuffer().BeginSuppressUndo()
        outfile.Freeze()
        
        # set global styling
        outfile.SetBackgroundColour(self.style.background_color)
        # set character style
        i = 0
        for token, text in tokensource:
            # get range
            rng = wx.wx_richtext.RichTextRange(i, i + len(text))
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
        # import wx
        import pygments.formatters.wx_richtext

        # if cached, return cached value
        if token in self._styles_cache:
            return self._styles_cache[token]
        # get style spec for token
        spec = self.style.style_for_token(token)
        # make base font
        font = wx.wx_richtext.RichTextAttr()
        font.Apply(base)
        # apply spec to font
        font.SetFontStyle(wx.FONTSTYLE_ITALIC if spec['italic'] else wx.FONTSTYLE_NORMAL)
        font.SetFontWeight(wx.FONTWEIGHT_BOLD if spec['bold'] else wx.FONTWEIGHT_NORMAL)
        font.SetFontUnderlined(wx.TEXT_ATTR_UNDERLINE_SOLID if spec['underline'] else wx.TEXT_ATTR_UNDERLINE_NONE)
        if spec['color'] is not None:
            font.SetTextColour(f"#{spec['color']}")
        # assign to styles dict
        self._styles_cache[token] = font

        return font