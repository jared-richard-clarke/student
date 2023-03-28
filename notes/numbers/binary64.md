# IEEE 754 Binary64 (Double)

Source: [Double-precision floating-point format](https://en.wikipedia.org/wiki/Double-precision_floating-point_format)

- Sign: 1 bit
- Exponent: 11 bits
- Significand (mantissa): 53 bits (52 explicitly stored)

![binary-64](https://user-images.githubusercontent.com/80301412/228359369-9090a259-e227-472e-8826-0e614069bc0f.png)

## Conversion

The real value assumed by a given 64-bit double precision datum with a given
biased exponent $e$ and a 52-bit fraction:

### Formula 1:

$$(-1)^{\text{sign}}(1.b_{51}b_{50}...b_{0})_{2}\times 2^{e-1023}$$

### Formula 2:

$$(-1)^{\text{sign}}\left(1+\sum _{i=1}^{52}b_{52-i}2^{-i}\right)\times 2^{e-1023}$$
