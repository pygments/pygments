#! /usr/bin/env python3

from pygments.token import Token


# rule components
Rule                        = Token.Kappa.Rule
Rule_Decor                  = Token.Kappa.Rule.Decor        # e.g. commas between agents
Rule_Agent                  = Token.Kappa.Rule.Agent        # whole agent
Agent_Name                  = Token.Kappa.Rule.Agent.Name       # agent's name
Agent_Oper                  = Token.Kappa.Rule.Agent.Operation  # edit operation following the parenthesis
Agent_Decor                 = Token.Kappa.Rule.Agent.Decorators # e.g. the parenthesis themselves
Agent_Sign                  = Token.Kappa.Rule.Agent.Signature  # the whole signature, sites plus commas
Agent_Sign_Decor            = Token.Kappa.Rule.Agent.Signature.Decorators   # the commas between sites

Agent_Site                  = Token.Kappa.Rule.Agent.Signature.Site         # a single site
Site_Name                   = Token.Kappa.Rule.Agent.Signature.Site.Name        # the site's name
Site_Bond                   = Token.Kappa.Rule.Agent.Signature.Site.Bond        # the site's bond data
Site_Bond_Decor             = Token.Kappa.Rule.Agent.Signature.Site.Bond.Decorators     # e.g. square brackets
Site_Bond_State             = Token.Kappa.Rule.Agent.Signature.Site.Bond.State          # bond identifier, or typing
Site_Bond_State_Agent       = Token.Kappa.Rule.Agent.Signature.Site.Bond.State.Agent    # agent in site.Agent
Site_Bond_State_Site        = Token.Kappa.Rule.Agent.Signature.Site.Bond.State.Site     # site in site.Agent
Site_Bond_Oper              = Token.Kappa.Rule.Agent.Signature.Site.Bond.Operation      # if edit operation, whole operation
Site_Bond_Oper_Decor        = Token.Kappa.Rule.Agent.Signature.Site.Bond.Operation.Decorators   # e.g. the slash
Site_Bond_Oper_State        = Token.Kappa.Rule.Agent.Signature.Site.Bond.Operation.States       # the before slash, and after slash components
Site_Bond_Oper_State_Agent  = Token.Kappa.Rule.Agent.Signature.Site.Bond.Operation.States.Agent # when typing in edit operation, the agent
Site_Bond_Oper_State_Site   = Token.Kappa.Rule.Agent.Signature.Site.Bond.Operation.States.Site  # when typing in edit operation, the site

Site_Int                    = Token.Kappa.Rule.Agent.Signature.Site.Internal    # the site's internal state data
Site_Int_Decor              = Token.Kappa.Rule.Agent.Signature.Site.Internal.Decorators # e.g. curly brackets
Site_Int_State              = Token.Kappa.Rule.Agent.Signature.Site.Internal.State      # internal state data
Site_Int_Oper               = Token.Kappa.Rule.Agent.Signature.Site.Internal.Operation  # if edit operation, whole expression
Site_Int_Oper_Decor         = Token.Kappa.Rule.Agent.Signature.Site.Internal.Operation.Decorators   # e.g. the slash
Site_Int_Oper_State         = Token.Kappa.Rule.Agent.Signature.Site.Internal.Operation.States       # the before slash, and after slash components

Site_Count                  = Token.Kappa.Rule.Agent.Signature.Site.Counter     # the site's counter data
Site_Count_Decor            = Token.Kappa.Rule.Agent.Signature.Site.Counter.Decorators  # e.g. curly brackets
Site_Count_State            = Token.Kappa.Rule.Agent.Signature.Site.Counter.State       # the counter state data
Site_Count_Oper             = Token.Kappa.Rule.Agent.Signature.Site.Counter.Operation   # if operation, whole expression
Site_Count_Oper_Decor       = Token.Kappa.Rule.Agent.Signature.Site.Counter.Operation.Decorators    # e.g. the slash
Site_Count_Oper_State       = Token.Kappa.Rule.Agent.Signature.Site.Counter.Operation.States        # the before slash, and after slash components

# declaration components
Declaration             = Token.Kappa.Declaration
Dec_Keyword             = Token.Kappa.Declaration.Keyword       # either %agent: or %token:
Dec_Ag                  = Token.Kappa.Declaration.Agent         # the kappa agent being declared
Dec_Ag_Name             = Token.Kappa.Declaration.Agent.Name            # the agent's name
Dec_Ag_Decor            = Token.Kappa.Declaration.Agent.Decoration      # the parenthesis
Dec_Ag_Sign             = Token.Kappa.Declaration.Agent.Signature       # the agent's signature
Dec_Ag_Sign_Decor       = Token.Kappa.Declaration.Agent.Signature.Decorator     # inter-site commas
Dec_Ag_Sign_Site        = Token.Kappa.Declaration.Agent.Signature.Site          # a site, with internal/bond/counter data
Dec_Ag_Sign_Site_Name   = Token.Kappa.Declaration.Agent.Signature.Site.Name         # a site's name
Dec_Ag_Sign_Site_Bd     = Token.Kappa.Declaration.Agent.Signature.Site.Bond         # type specifier, i.e: [site.Agent]
Dec_Ag_Sign_Site_Bd_s   = Token.Kappa.Declaration.Agent.Signature.Site.Bond.Site        # site in [site.Agent]
Dec_Ag_Sign_Site_Bd_a   = Token.Kappa.Declaration.Agent.Signature.Site.Bond.Agent       # Agent in [site.Agent]
Dec_Ag_Sign_Site_Bd_d   = Token.Kappa.Declaration.Agent.Signature.Site.Bond.Decorator   # brackets and period in [site.Agent]
Dec_Ag_Sign_Site_In_s   = Token.Kappa.Declaration.Agent.Signature.Site.Internal.State       # un ph in {un, ph}
Dec_Ag_Sign_Site_In_d   = Token.Kappa.Declaration.Agent.Signature.Site.Internal.Decorator   # comma and curlies in {un, ph}
Dec_Ag_Sign_Site_Ct_s   = Token.Kappa.Declaration.Agent.Signature.Site.Counter.State            # 1 and 5 in {=1/+=5}
Dec_Ag_Sign_Site_Ct_d   = Token.Kappa.Declaration.Agent.Signature.Site.Counter.Decorator        # equals, slash, curlies in {=1/+=5}

# perturbations
Perturbation    = Token.Kappa.Perturbation              # the whole perturbation syntax (minus agent usage)
Pert_Keyword    = Token.Kappa.Perturbation.Keyword      # %mod:
Pert_Decor      = Token.Kappa.Perturbation.Decorators   # do, repeat
Pert_Oper       = Token.Kappa.Perturbation.Operation    # e.g. STOP, SNAPSHOT, ADD
Pert_Constructs = Token.Kappa.Perturbation.Constructs   # e.g. [E], [T], alarm
Pert_FileName   = Token.Kappa.Perturbation.FileName     # double-quoted string used for filenames

# other script components
Alge_Oper       = Token.Kappa.Operand               # algebraic operations: + - * / ^
Misc_Keyword    = Token.Kappa.Miscelaneous          # e.g. %obs:, %init:, %var:
Misc_Func       = Token.Kappa.Function              # e.g. exp, sqrt, max, cos
Comment         = Token.Kappa.Comment               # comments
Whitespace      = Token.Kappa.Whitespace
Number          = Token.Kappa.Number
String          = Token.Kappa.String                # Unquoted and single-quoted identifiers, like rule names