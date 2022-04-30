call.values = vector(mode="numeric")
asset.price = vector(mode="numeric")
for (s in 75:130){
  result <- bscall.value(s,100,0.75,0.02,0.23)
  call.values <- append(call.values,result)
  result1 <- s
  asset.price = append(asset.price,result1)
}
plot(asset.price,call.values)

