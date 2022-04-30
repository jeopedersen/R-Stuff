put.values = vector(mode="numeric")
asset.price = vector(mode="numeric")
for (s in 1:120){
  result <- bsput.value(s,100,0.75,0.02,0.23)
  put.values <- append(put.values,result)
  result1 <- s
  asset.price = append(asset.price,result1)
}
plot(asset.price,put.values)
