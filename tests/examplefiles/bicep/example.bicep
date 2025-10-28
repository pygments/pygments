// Example bicep file for deploying storage to Azure
// TODO: add more validation on the storage account name

targetScope = 'resourceGroup'

// basic metadata - might want to add more later
metadata name = 'Azure Storage Example'
metadata description = 'Deploys storage account with various features'
metadata version = '1.0.0'

// Parameters section
// most of these are standard storage params

@description('Storage Account type')
@allowed([
  'Standard_LRS'
  'Standard_GRS'
  'Standard_ZRS'
  'Premium_LRS'
])
param storageAccountType string = 'Standard_LRS'

@description('Location for all resources.')
@minLength(3)
@maxLength(24)
param location string = resourceGroup().location

@secure()
@description('The administrator password')
param adminPassword string

// this controls how many storage accounts to create in the loop below
@minValue(1)
@maxValue(100)
param instanceCount int = 3

// Variables

var uniqueStorageName = 'store${uniqueString(resourceGroup().id)}'
var tags = {
  environment: 'production'
  costCenter: 'IT'
  project: 'Infrastructure'
}

// string interpolation example
var connectionString = 'Server=${sqlServer.properties.fullyQualifiedDomainName};Database=${databaseName}'

// Multi-line string
// multi-line string for deployment script
var scriptContent = '''
#!/bin/bash
echo "Hello World"
echo "This is a multi-line script"
'''

// User-defined types - these help with validation

@description('Configuration for network settings')
type networkConfig = {
  @description('Virtual network name')
  vnetName: string

  @description('Address prefix')
  @minLength(9)
  addressPrefix: string

  @description('Subnet configuration')
  subnet: {
    name: string
    addressPrefix: string
  }
}

// union type for SKU options
type skuType = 'Basic' | 'Standard' | 'Premium'

// Main storage account resource

@description('Storage account for data')
resource storageAccount 'Microsoft.Storage/storageAccounts@2023-04-01' = {
  name: uniqueStorageName
  location: location
  tags: tags
  sku: {
    name: storageAccountType
  }
  kind: 'StorageV2'
  properties: {
    supportsHttpsTrafficOnly: true
    minimumTlsVersion: 'TLS1_2'
    encryption: {
      services: {
        blob: {
          enabled: true
        }
        file: {
          enabled: true
        }
      }
      keySource: 'Microsoft.Storage'
    }
  }
}

// blob service configuration
resource blobService 'Microsoft.Storage/storageAccounts/blobServices@2023-04-01' = {
  parent: storageAccount
  name: 'default'
  properties: {
    deleteRetentionPolicy: {
      enabled: true
      days: 7
    }
  }
}

// container for storing blobs
resource container 'Microsoft.Storage/storageAccounts/blobServices/containers@2023-04-01' = {
  parent: blobService
  name: 'data'
  properties: {
    publicAccess: 'None'
  }
}

// NOTE: this is only deployed if enableDiagnostics is true
resource diagnosticSettings 'Microsoft.Insights/diagnosticSettings@2021-05-01-preview' = if (enableDiagnostics) {
  name: 'diagSettings'
  scope: storageAccount
  properties: {
    workspaceId: logAnalyticsWorkspace.id
    logs: []
    metrics: [
      {
        category: 'Transaction'
        enabled: true
      }
    ]
  }
}

// create multiple storage accounts using a loop
resource storageAccounts 'Microsoft.Storage/storageAccounts@2023-04-01' = [for i in range(0, instanceCount): {
  name: '${uniqueStorageName}${i}'
  location: location
  sku: {
    name: storageAccountType
  }
  kind: 'StorageV2'
  properties: {}
}]

// Existing resource reference
resource existingVnet 'Microsoft.Network/virtualNetworks@2023-05-01' existing = {
  name: 'my-existing-vnet'
  scope: resourceGroup('networking-rg')
}

// modules section - seperate files for network stuff
// TODO: probably should add monitoring module too

@description('Deploy network resources')
module networkModule './modules/network.bicep' = {
  name: 'networkDeploy'
  params: {
    vnetName: 'myVnet'
    location: location
    addressPrefix: '10.0.0.0/16'
  }
  dependsOn: [
    storageAccount  // wait for storage first
  ]
}

// module with subscription scope instead of rg
module subscriptionModule './modules/subscription.bicep' = {
  name: 'subDeploy'
  scope: subscription()
  params: {
    resourceGroupName: 'new-rg'
  }
}

// deploy same module to multiple regions
@batchSize(2)  // only deploy 2 at a time so we dont hit limits
module regionModules './modules/region.bicep' = [for region in regions: {
  name: 'deploy-${region}'
  scope: resourceGroup(region)
  params: {
    location: region
  }
}]

// custom functions
// these are pretty handy for reusuing logic

@description('Generates a unique name')
@export()
func generateUniqueName(prefix string, suffix string) string => '${prefix}-${uniqueString(subscription().id)}-${suffix}'

@export()
func calculateTotal(price int, quantity int) int => price * quantity  // simple math helper

// outputs - what we want to return from this deployment

@description('Storage account name')
output storageAccountName string = storageAccount.name

@description('Storage account ID')
output storageAccountId string = storageAccount.id

@description('Primary endpoints')
output primaryEndpoints object = storageAccount.properties.primaryEndpoints

@description('Blob endpoint')
output blobEndpoint string = storageAccount.properties.primaryEndpoints.blob

@secure()  // make sure this doesnt show up in logs!
output storageConnectionString string = 'DefaultEndpointsProtocol=https;AccountName=${storageAccount.name};AccountKey=${storageAccount.listKeys().keys[0].value}'

// safe navigation and default values incase the property dosent exist
output optionalValue string = storageAccount.properties.?customProperty ?? 'default-value'

// output an array with indicies
output storageAccountNames array = [for (name, i) in storageAccountNames: {
  index: i
  name: name
}]


// advanced stuff and edge cases
// some of these might not work in older bicep versions

// ternary operator - basic conditional
var isProduction = environment == 'prod' ? true : false

// combining logical operators - be careful with precedence here
var shouldDeploy = isProduction && enableFeature || overrideFlag

// lambda functions with arrays
var filteredItems = filter(items, item => item.enabled)  // only get enabled ones
var mappedItems = map(items, item => item.name)  // extract just names

// safe dereference - wont throw error if property missing
var optionalProperty = myObject.?optionalProperty

// coalesce operator for fallback values
var valueOrDefault = myVariable ?? 'defaultValue'

// lambda expression in custom function
func processItems(items array) array => map(items, item => {
  name: item.name
  value: item.value * 2  // double the value
})

/*
  Multi-line comment example
  this demonstrates that multi-line comments work corectly
  and can span multiple lines
  useful for longer explanations or TODOs
*/

// import statements - newer bicep feature
import * from './types.bicep'  // import everything
import { myFunction, myType } from './shared.bicep'  // import specific items

// using statement - for parameter files
using './main.bicep'

// assert to validate conditions at compile time
assert myCondition = length(items) > 0

// provider and extension for bicep extensibility
extension kubernetes with {
  kubeConfig: kubeConfigContent  // TODO: make sure this is set properly
  namespace: 'default'
}

provider microsoftGraph  // enables graph resources

// string escape sequences
var escapedString = 'Line 1\nLine 2\tTabbed\r\nWindows line ending'
var quotedString = 'He said \'Hello\''  // escaped quotes
var unicodeString = 'Unicode: \u0048\u0065\u006C\u006C\u006F'  // unicode escapes

