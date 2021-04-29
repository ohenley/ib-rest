# ib-ada
Interactive Brokers (IB) RESTful server written in Ada.

## Table of Contents
<details>
<summary>Click to expand</summary>

1. [About](#About)
2. [Status](#Status)
3. [Prerequisites](#Prerequisites)  
4. [Dependencies](#Dependencies)
5. [Building](#Building)
   1. [Windows](#Windows)
   2. [Other OSes](#Other-OSes)
6. [Installation](#Installation)
7. [Limitations](#Limitations)
8. [Usage](#Usage)
   1. [Available HTTP endpoints](#Available-HTTP-endpoints)
10. [Acknowledgments](#Acknowledgments)

</details>

## About
- ib-rest is a representational state transfer (REST) server offering the ability to trade, monitor, and manage one's Interactive Brokers (IB) account using a simple RESTful API.

- ib-rest, and all its dependencies, are 100% Ada code but the RESTful API it exposes makes it exploitable from any other programming language or technology that supports HTTP communication.

- I started this project because every other 'equivalent' implementation (IB API implementation of all kinds) I tested did not work for my use case, for one reason or another.

- ib-rest really is just a convenient RESTful layer over the more involved [ib-ada](https://github.com/ohenley/ib-ada) communication engine. For a more thorough discussion on the technical implications of interfacing IB technologies please consult its [readme](https://github.com/ohenley/ib-ada).

## Status
- First. This application should work out of the box as I am using it daily. My experience is that it is debugged and quite robust as-is.
- Being lean and mean, it has a limited scope. See [Limitations](#Limitations).
- The available endpoints are documented in the [Usage](#Usage) section.

## Prerequisites
- An activated Interactive Brokers (IB) account. I recommend a 'paper' account (fake account) at first.
- Win32 or Linux platform (tested and working on Windows 10, Lubuntu 20.04.1)
- [TWS](https://www.interactivebrokers.ca/en/index.php?f=16040) or [IB Gateway](https://www.interactivebrokers.ca/en/index.php?f=16457) v978+.
- GNAT (tested and working with GNAT community 2020, GNAT FSF 9.3.0)

## Dependencies
- [ib-ada](https://github.com/ohenley/ib-ada) - IB TWS/IB Gateway communication engine library.
- [black](https://github.com/ohenley/black) - HTTP library.

## Building

*IMPORTANT* : This project uses git submodules. Clone accordingly, like follow:

#### Windows
- Install [GNAT community 2020](https://community.download.adacore.com/v1/966801764ae160828c97d2c33000e9feb08d4cce?filename=gnat-2020-20200429-x86_64-windows-bin.exe)
```
$ git clone --recursive https://github.com/ohenley/ib-rest.git    
$ cd ib-rest
$ gprbuild ib_rest.gpr
```
#### Linux (ubuntu 20.04.1+ flavors)
```
$ sudo apt-get install gnat-gps
$ git clone --recursive https://github.com/ohenley/ib-rest.git
$ cd ib-rest
$ gprbuild ib_rest.gpr
```
## Installation
Not Applicable.

## Limitations
Only works for stocks (STK) and provides a minimum viable interface to the TWS/IB Gateway for a typical trading bot. Complete but no fancy, namely:

- accounts information (account ids and different balance types)
- positions (with-profits)
- commission
- place orders
- open orders
- cancel_orders

## Usage
- Start TWS or IB Gateway.

First command prompt:
```
$ cd [ib-rest_root_folder]/build/bin
$ ./rest_server run port:8080 gateway:ib_paper
```
Second command prompt:
```
$ cd [ib-rest_root_folder]/tests
$ python3 ./python-client.py
```
All responses are JSON objects and follow the [JSend](https://github.com/omniti-labs/jsend) response convention. 

#### Available HTTP endpoints
- **accounts information** : `http://{base_url}/accounts_summary?tag={tag_type}` 
    - description : retrieves type of account, account currency, different amounts type.   
    - mandatory : [`tag`](https://github.com/ohenley/ib-ada/blob/main/src/ib_ada.ads#L214-L224)
    - e.g. : 
      - request : `http://127.0.0.1:8080/accounts_summary?tag=NET_LIQUIDATION`
      - response : 
         ```
         {
            "data": {
               "accounts": {
                  "DU3689338": {
                     "open_orders": [],
                     "positions": {},
                     "summaries": {
                        "net_liquidation": {
                           "currency": "USD",
                           "value": 998645.0
                        }
                     }
                  }
               }
            },
            "request_number": 0,
            "status": "success"
         }
         ```

- **positions (with profits)** : `http://{base_url}/positions?profit_and_loss={boolean}`
    - description : retrieves your current positions (trades) data.     
    - optional : `profit_and_loss`
    - e.g. : 
      - request : `http://127.0.0.1:8080/positions?profit_and_loss=true`
      - response : 
         ```
         {
            "data": {
                "accounts": {
                    "DU3689338": {
                        "open_orders": [],
                        "positions": {
                            "ALTO": {
                                "average_cost": 5.07503,
                                "contract": {
                                    "contract_id": 465324096,
                                    "currency": "USD",
                                    "exchange": "NASDAQ",
                                    "security": "STK",
                                    "symbol": "ALTO"
                                },
                                "open_value": 1009.93,
                                "quantity": 199,
                                "unrealized_profit": 162.976
                            },
                            ...
                            "CRCT": {
                                "average_cost": 25.6863,
                                "contract": {
                                    "contract_id": 478807161,
                                    "currency": "USD",
                                    "exchange": "NASDAQ",
                                    "security": "STK",
                                    "symbol": "CRCT"
                                },
                                "open_value": 976.08,
                                "quantity": 38,
                                "unrealized_profit": -11.3875
                            }
                        },
                        "summaries": {}
                    }
                }
            },
            "request_number": 0,
            "status": "success"
        }
         ```

- **commission** : `http://{base_url}/commission?side={order_side_type}&symbol={string}&quantity={integer}&at_price_type={order_at_price_type}`
    - description : retrieves how much it would cost to enter a position (trade).
    - mandatory : [`side`](https://github.com/ohenley/ib-ada/blob/main/src/ib_ada.ads#L77)
    - mandatory : `symbol`
    - mandatory : `quantity`
    - mandatory : [`at_price_type`](https://github.com/ohenley/ib-ada/blob/main/src/ib_ada.ads#L78) 
    - e.g. : 
      - request : `http://127.0.0.1:8080/commission?side=BUY&symbol=IBM&quantity=10&at_price_type=MKT`
      - response : 
         ```
         {
            "data": {
               "commission": 0.99
            },
            "request_number": 0,
            "status": "success"
         }
         ```

- **place orders** : `http://{base_url}/place_order?side={order_side_type}&symbol={string}&quantity={integer}&at_price_type={order_at_price_type}`
    - description : enter a trade for a given security.
    - mandatory : [`side`](https://github.com/ohenley/ib-ada/blob/main/src/ib_ada.ads#L77)
    - mandatory : `symbol`
    - mandatory : `quantity`
    - mandatory : [`at_price_type`](https://github.com/ohenley/ib-ada/blob/main/src/ib_ada.ads#L78) 
    - e.g. : 
      - request : `http://127.0.0.1:8080/place_order?side=BUY&symbol=IBM&quantity=10&at_price_type=MIDPRICE`
      - response : 
         ```
         {
            "data": {},
            "request_number": 11804,
            "status": "success"
         }
         ```
    - note: in theory should be a POST call (todo) but it changes nothing in reality, there is no GET/POST police yet.

- **open orders** : `http://{base_url}/open_orders`
    - description : retrieves trades already ordered to IB but not yet sealed/completed.
    - e.g. : 
      - request : `http://127.0.0.1:8080/open_orders`
      - response : 
         ```
         {
            "data": {
               "accounts": {
                  "DU3689338": {
                     "open_orders": [
                        {
                           "contract": {
                              "contract_id": 8314,
                              "currency": "USD",
                              "exchange": "SMART",
                              "security": "STK",
                              "symbol": "IBM"
                           },
                           "order": {
                              "at_price_type": "MIDPRICE",
                              "quantity": " 10",
                              "side": "BUY",
                              "time_in_force": "DAY"
                           },
                           "request_number": 11804
                        }
                     ],
                  "positions": {},
                  "summaries": {}
                 }
               }
            },
            "request_number": 0,
            "status": "success"
         }
         ```

- **cancel_orders** : `http://{base_url}/cancel_order?request_number={integer}`
    - description : cancel a trade already ordered to IB.
    - mandatory : `request_number`
    - e.g. : 
      - request : `http://127.0.0.1:8080/cancel_order?request_number=11804`
      - response : 
         ```
         {
            "data": "cancel order 11804 sent.",
            "request_number": 0,
            "status": "success"
         }
         ```
    - note: in theory should be a POST call (todo) but it changes nothing in reality, there is no GET/POST police yet.


## Acknowledgments
- Thanks to remembered @sparre for his wonderful and sane work on [black](https://github.com/sparre/black) which made the REST layer a breeze to make.
