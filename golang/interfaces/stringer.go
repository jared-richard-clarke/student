// The "fmt" package looks for the "Stringer" interface when printing values.
// The "Stringer" interface as defined in the "fmt" package:
// type Stringer interface {
// 	String() string
// }
// Example pulled from "https://go.dev/tour/methods/18"

package main

import "fmt"

type IPAddr [4]byte

func (ip IPAddr) String() string {
	return fmt.Sprintf("%v.%v.%v.%v", ip[0], ip[1], ip[2], ip[3])
}
func main() {
	hosts := map[string]IPAddr{
		"loopback":  {127, 0, 0, 1},
		"googleDNS": {8, 8, 8, 8},
	}
	for name, ip := range hosts {
		fmt.Printf("%v: %v\n", name, ip)
	}
}

// === without stringer method ===
// loopback: [127 0 0 1]
// googleDNS: [8 8 8 8]

// === with stringer method ===
// loopback: 127.0.0.1
// googleDNS: 8.8.8.8
