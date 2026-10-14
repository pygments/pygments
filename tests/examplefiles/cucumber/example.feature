# First comment
Feature: My amazing feature
  Feature description line 1
  Feature description line 2

#comment
Scenario Outline: My detailed scenario #string
  Given That <x> is set
  When When I <subtract>
  Then I should get the <remain#der>

  # indented comment
  Examples:
    | x    | subtract | remain#der |
    | 12   | 5\|3     |  #73       |
    | #the | 10       |  15        |

Rule: grouping scenarios
  Example: an example is a scenario
    Given a precondition
    When an action
    Then a result

  Scenario Template: template with an examples table
    Given a <thing>
    Then it is <state>
    Scenarios:
      | thing | state |
      | x     | ok    |
