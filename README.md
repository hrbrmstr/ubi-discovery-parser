# ubi-discovery-parser

Parser for the output of Ubiquiti device discovery responses on UDP 10001

Also a fairly decent example of how to work with raw data in R.

## Scanning from R

``` r
source("ubi-discovery-scanner.R")
source("ubi-discovery-parser.R")

res <- ubnt_discovery("IP ADDRESS OF TARGET", 10001, c(0x01, 0x00, 0x00, 0x00))

ubi_parse_response(res);
```

## Using already captured data

``` r
source("ubi-discovery-parser.R")

c(
  "AQAAggIACiSkPH9XF8+QRUICAAokpDx+VxfAqAUBAQAGJKQ8flcXCgAEACylqgsADEFpclJvdXRlciBIUAwABkxBUC1IUA0AD1dDVGVsLVdpRmktNTcxNw4AAQMDACJYTS5hcjcyNDAudjUuNS42LjE3NzYyLjEzMDUyOC4xNzU1EAAC5LI=",
  "AQAAsAIACiSkPGjsr6pRS8kCAAokpDxp7K/AqAEBAQAGJKQ8aOyvCgAEABdMoAsAHGNsaWVudGVfTUFSSUFfTE9VUkRFX0lUQVRVQkEMAANMTTUNABpTU0lORVRfRVhUUkVNRV9JVEFUVUJBXzAwMQ4AAQIDACJYTS5hcjcyNDAudjUuNi41LjI5MDMzLjE2MDUxNS4yMTE5EAAC6KUUABNOYW5vU3RhdGlvbiBMb2NvIE01",
  "AQAAqAIACgQY1jaL7L5sKNgCAAoEGNY3i+zAqDIBAQAGBBjWNovsCgAEAABgKgsAKEhBQ0tFRC1ST1VURVItSEVMUC1TT1MtSEFELURVUEUtUEFTU1dPUkQMAAZONUItMTkNAAhDT1NXT1JUSA4AAQIDACJYVy5hcjkzNHgudjUuNi42LjI5MTgzLjE2MDUyNi4xMjA1EAAC6CUUAA5OYW5vQmVhbSBNNSAxOQ==",
  "AQAAgQIACtyf2+Jdpx+GN/YCAArcn9vjXacKCgoBAQAG3J/b4l2nCgAEAKnfkQsADEdvxYJ1Y2ggNjE2MQwAA0xNNQ0AEEFUbmV0LmplZS5wbDE2LkEOAAECAwAjWE0uYXI3MjQwLnY1LjUuMTAuMjQyNDEuMTQxMDAxLjE2NDkQAALopQ==",
  "AQAAiwIACiSkPIiAfqf5VigCAAokpDyJgH4KCgACAQAGJKQ8iIB+CgAEAAVhHQsABk1hcmlzYQwABkFHNS1IUA0ADkxpbmtUYXAwMi01R2h6DgABAgMAIlhNLmFyNzI0MC52NS42LjkuMjk1NDYuMTYwODE5LjExNTcQAALiVRQADUFpckdyaWQgTTUgSFA=",
  "AQAAjwIACgAnIpiNE4OhiVcCAAoAJyKZjRPAqAEUAgAKACcimY0Tqf6NEwEABgAnIpiNEwoABAALmrMLAA5IZWxpbyBNaWxhZ3JlcwwAA0xDNQ0AEEZhemVuZGEgUGVnYSBCZW0OAAECAwAiWFM1LmFyMjMxMy52NC4wLjQuNTA3NC4xNTA3MjQuMTM0NBAAAsEF"
) -> test_payloads

lapply(test_payloads, ubi_parse_response)
## [[1]]
## [Model: LAP-HP; Firmware: XM.ar7240.v5.5.6.17762.130528.1755; Uptime: 33.9 (hrs)
##
## [[2]]
## [Model: LM5; Firmware: XM.ar7240.v5.6.5.29033.160515.2119; Uptime: 17.7 (hrs)
##
## [[3]]
## [Model: N5B-19; Firmware: XW.ar934x.v5.6.6.29183.160526.1205; Uptime: 0.3 (hrs)
##
## [[4]]
## [Model: LM5; Firmware: XM.ar7240.v5.5.10.24241.141001.1649; Uptime: 128.9 (hrs)
##
## [[5]]
## [Model: AG5-HP; Firmware: XM.ar7240.v5.6.9.29546.160819.1157; Uptime: 4.1 (hrs)
##
## [[6]]
## [Model: LC5; Firmware: XS5.ar2313.v4.0.4.5074.150724.1344; Uptime: 8.8 (hrs)
```