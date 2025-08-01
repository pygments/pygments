local json = require('json')
local scriptmanager = require('script-manager')
local path = scriptmanager.getModStatePath('mymodname')
config = config or json.open(path .. 'settings.json')

-- modify state in the config.data table and persist it when it changes with
-- config:write()