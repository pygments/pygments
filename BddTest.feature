Feature: Checkout on Shopping
  Scenario Outline: Checkout Shirt 
    Given the price of a "Shirt" is 20.5RMB 
    When I checkout <count> "Shirt"
    Then the <total> price should be 20.5RMB

    Examples:
    | count | total    |     
    | 1     | 20.5     | 
    | 3     | 61.5     |

  Scenario: Two Shirt scanned separately 
    Given the price of a "Shirt" is 300RMB 
    When I checkout 1 "Shirt"
    And I checkout 1 "Shirt"
    Then the total price should be 600RMB

  Scenario: A Shirt and an Shoes
    Given the price of a "Shirt" is 200RMB
    And the price of a "Shoes" is 300RMB 
    When I checkout 1 "Shirt"
    And I checkout 1 "Shoes"
    Then the total price should be 500RMB