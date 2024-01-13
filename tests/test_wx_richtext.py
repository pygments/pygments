"""
    Test suite for the wx richtext formatter
"""

import time
import threading
import pygments, pygments.lexers, pygments.formatters


app = frame = None


def close_app_after(t=1):
    """ 
    Close the app after t seconds
    """
    # get app
    global app
    # sleep
    time.sleep(t)
    # close app
    app.ExitMainLoop()
    


def setup_wx_richtext():
    """
    Setup a basic wx app with a styled rich text ctrl. Returns the ctrl and its formatter.
    """
    # skip test if wx is not installed (mostly for local use)
    try:
        import wx 
    except ImportError:
        import pytest
        pytest.skip(reason="Test requires wx.")
        return
    # if we have wx, import richtext
    import wx.richtext

    # make an app with just a basic frame
    global app, frame
    app = wx.App(False)
    frame = wx.Frame(
        parent=None, 
        size=(720, 720)
    )
    frame.SetSizer(wx.BoxSizer(wx.VERTICAL))
    frame.Show()
    # write some example Python code
    # (taken from w3schools' "Python Classes And Objects: Create object methods" demo)
    example_str = """class Person:
    def __init__(self, name, age):
        self.name = name
        self.age = age

    def myfunc(self):
        print("Hello my name is " + self.name)

    p1 = Person("John", 36)
    p1.myfunc()"""
    # make a wx.rtc formatter
    formatter = pygments.formatters.get_formatter_by_name("wx.rtc", style="vs")
    # make a rich text ctrl with the example code
    ctrl = wx.richtext.RichTextCtrl(
        frame, 
        value=example_str, 
        size=(720, 720)
    )
    # make a pygments lexer
    lexer = pygments.lexers.get_lexer_by_name("python")
    # lex content for tokens
    tokens = pygments.lex(ctrl.GetValue(), lexer=lexer)
    # format ctrl using tokens
    formatter.format(tokensource=tokens, outfile=ctrl)
    # add ctrl to frame sizer
    frame.GetSizer().Add(ctrl, flag=wx.EXPAND)


    return ctrl, formatter


def test_setup():
    """
    Tests that the formatter can be applied without error 
    """
    # setup app
    ctrl, formatter = setup_wx_richtext()
    # setup app to stop after 1s
    app_thread = threading.Thread(target=close_app_after)
    app_thread.start()
    # start app
    global app
    app.MainLoop()
    
    
test_setup()