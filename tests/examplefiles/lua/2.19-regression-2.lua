local function bribe()
    tes3ui.showInventorySelectMenu({
        callback = function(e)
            if e.item then
                local itemWorth = e.item.value * e.count
                -- At last! Now the actual persuasion part. We use `modifier` argument.
            end
        end,
    })
end
