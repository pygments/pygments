#! /usr/bin/env python3

from pygments.style import Style
from pygments.token import Comment, Number, String, Operator
from pygments.token_kappa import *

"""Kappa has a lot of idiosyncratic components; a lot of those have a Pygments-token associated to them.
Refer to those defined in kappa_tokens.py for documentation and hierarchy."""

class DemoStyle(Style):
    """This style showcases some of the subtleties in the lexer."""
    default_style = ''
    styles = {
        Agent_Name: 'bold',
        Site_Bond_State_Agent: 'bold',
        Agent_Oper: 'underline #f00',
        Site_Bond: '#cc00cc',
        Site_Int: '#0000ff',
        Site_Count: '#009999',
        String: 'italic',
        Site_Bond_Oper: 'underline',
        Site_Int_Oper: 'underline',
        Site_Count_Oper: 'underline',
        Dec_Ag_Sign_Site_Bd: '#999999',
        Pert_Keyword: 'bold',
        Pert_Decor: 'italic',
        Pert_Oper: 'bg:#ffcccc',
        Comment: 'bg:#f2f2f2 italic',
        Number: '#cc7a00',
        Dec_Keyword: 'bold',
        Misc_Keyword: 'bold',
        Rule_Decor: '#009900',
        # Comment: '#f00',
        # Number: '#f00',
        # String: '#f00',
        # Number: '#f00',
        # Operator: '#f00'
    }


class KaSimInBrowserStyle(Style):
    """This style mimics the CodeMirror interface used in the GUI / KappApp."""
    default_style = ''
    styles = {
        Comment: '#a50',
        Misc_Keyword: '#708',
        Dec_Keyword: '#708',
        Pert_Keyword: '#708',
        Agent_Decor: '#05a',
        Dec_Ag_Decor: '#05a',
        Rule_Decor: '#05a',
        Number: '#164',
        Operator: '#05a',
        Misc_Func: '#05a',
        Pert_Oper: '#05a',
        Pert_Decor: '#05a',
        Pert_FileName: '#a11',
        Pert_Constructs: '#708',
    }

class EditNotationDeltasStyle(Style):
    """This style highlights edit notation operations."""
    default_style = ''
    styles = {
        String: '#808080 italic',
        Agent_Name: 'bold',
        Site_Bond_Oper: 'bg:#c9f2d6',
        Site_Int_Oper: 'bg:#c9e4f2',
        Agent_Oper: 'bg:#f2c9e4',
        Site_Count_Oper: 'bg:#f2d6c9',
        Rule_Decor: '#cc0000',
        Comment: 'bg:#b3b3b3'
    }