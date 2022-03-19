# Goroutines and Concurrency

## Channels
```go
channel := make(chan int)
channel <- value   // send value to channel
value := <-channel // receive from channel and assign to value
```
