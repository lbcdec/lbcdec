function test(obj)
    return obj:GetWidgetPowerValue() ~= 0 and obj:GetGameObjectInSlot(Slot.Containment, 0) == nil
end
