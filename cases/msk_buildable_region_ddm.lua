local function proto0(self, show)
    if show then
        local player = Universe:GetPlayerGameObject()
        local region = player:GetClosestBuildableRegion(10.0)
        Classes.DebugDisplayManager:Instance(BuildableRegion.kDebugTextStartX, BuildableRegion.kDebugTextStartY, BuildableRegion.kDebugTextContextName, {region})
    else
        local instance = Classes.DebugDisplayManager.GetInstance(BuildableRegion.kDebugTextContextName)
        if instance ~= nil then
            instance:Destroy()
        end
    end
end
