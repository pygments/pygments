// Parameters with decorators
@description('The location of the resource')
@allowed([
  'westus'
  'eastus'
])
param location string = 'westus'

@description('Storage account name')
@minLength(3)
@maxLength(24)
param storageAccountName string

@secure()
param adminPassword string

// Variables
var storageAccountType = 'Standard_LRS'
var locationLower = toLower(location)

// Resource with string interpolation
resource storageAccount 'Microsoft.Storage/storageAccounts@2023-01-01' = {
  name: '${storageAccountName}${uniqueString(resourceGroup().id)}'
  location: location
  sku: {
    name: storageAccountType
  }
  kind: 'StorageV2'
  properties: {
    supportsHttpsTrafficOnly: true
    encryption: {
      keySource: 'Microsoft.Storage'
    }
  }
  tags: {
    environment: 'production'
    costCenter: 'engineering'
  }
}

// Conditional resource
resource virtualNetwork 'Microsoft.Network/virtualNetworks@2023-04-01' = if (deployVnet) {
  name: vnetName
  location: location
  properties: {
    addressSpace: {
      addressPrefixes: [
        vnetAddressPrefix
      ]
    }
  }
}

// Module with loop
module appServiceModules './appService.bicep' = [for (site, i) in appServices: {
  name: '${site.name}-module-${i}'
  params: {
    appServiceName: site.name
    location: location
  }
}]

// Output with loop
output storageId array = [for i in range(0, 2): storageAccount.id]

// Complex expression
output connectionString string = 'DefaultEndpointsProtocol=https;AccountName=${storageAccount.name};AccountKey=${listKeys(storageAccount.id, storageAccount.apiVersion).keys[0].value}'

// Import and using
import * as mod from 'types.bicep'
using 'main.bicep' with {
  params: {
    name: 'test'
  }
}

// Comments test
/* Multi-line comment
   spanning multiple lines
   with various symbols: $ { } [ ] ( ) */

// Test various tokens
var testNumber = 123
var testBool = true
var testNull = null
var testObject = {
  prop1: 'value1'
  prop2: 42
}
var testArray = [
  'item1'
  'item2'
]