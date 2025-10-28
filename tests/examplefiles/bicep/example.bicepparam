// bicep parameters file
// these go with the main.bicep deployment

using './main.bicep'

// basic params
param location = 'eastus'  // could also use westus or centralus
param storageAccountType = 'Standard_LRS'
param instanceCount = 5  // how many instances to deploy

// secure parameter - dont hardcode passwords!
// this reads from environment variable instead
param adminPassword = readEnvironmentVariable('ADMIN_PASSWORD')

// object param for network configuration
param networkConfig = {
  vnetName: 'production-vnet'
  addressPrefix: '10.0.0.0/16'
  subnet: {
    name: 'default'
    addressPrefix: '10.0.1.0/24'  // make sure this is within vnet range
  }
}

// array of allowed IP ranges
param allowedIpAddresses = [
  '10.0.0.0/8'
  '172.16.0.0/12'
  '192.168.0.0/16'  // standard private ranges
]

// boolean flag
param enableDiagnostics = true  // set to false if you dont need diagnostics

// multi-line string for deployment scripts
param deploymentScript = '''
#!/bin/bash
echo "Starting deployment"
az --version
echo "Deployment complete"
'''

param environment = 'production'  // could be 'staging' or 'development'
param region = 'westSus'  // deployment region

// string interpolation works here too
param resourceNamePrefix = 'myapp-${environment}-${region}'  // dynamically build names

// more comments examples
/*
  Multi-line comments work fine
  in parameter files just like regualr bicep files
  usefull for longer notes
*/

