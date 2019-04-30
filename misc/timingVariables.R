itemBlankRatio = 1.5

times <- expand.grid(
  itemRate = 1/c(6,8,12,24), 
  blankDuration = numeric(1),
  itemDuration = numeric(1)
)

for(thisItemRate in c(6,8,12,24)){
  itemBlankDuration = 1/thisItemRate
  
  thisItemDuration = (itemBlankRatio/(itemBlankRatio+1))*itemBlankDuration
  thisBlankDuration = itemBlankDuration - thisItemDuration
  print(thisItemDuration*1000)
  
  times %<>% 
    mutate(blankDuration = replace(blankDuration, itemRate == itemBlankDuration, thisBlankDuration),
           itemDuration = replace(itemDuration, itemRate == itemBlankDuration, thisItemDuration))
}

