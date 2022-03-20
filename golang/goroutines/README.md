# Goroutines and Concurrency

## Resources
- [Communicating Sequential Processes](https://en.wikipedia.org/wiki/Communicating_sequential_processes)

## Goroutine
A goroutine is an independently-executing function launched by a Go statement. 
It has its own callstack, which grows and shrinks as needed.

## Concurrency

> "Don't communicate by sharing memory, share memory by communicating" — Rob Pike

> "Concurrency is the composition of independently executing computations ...
>  Concurrency is not parallelism, although it enables parallelism." 
> — Rob Pike

## Details of Parallelism
- threads
- semaphores
- locks
- barriers

## Channels
Channels are typed conduits through which goroutines send and receive values. 
Channels allow goroutines to communicate and synchronize.

Go channels can use buffers, although buffering removes synchronization.
```go
channel := make(chan int)
channel <- value   // send value to channel
value := <-channel // receive from value channel and assign to variable
```
