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
9. [Acknowledgments](#Acknowledgments)

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
- cancel_orders
- open orders

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

#### Available endpoints
- accounts information : 
    - description : retrieves type of account, account currency, different amounts type.
    - pattern : `http://{base_url}/accounts_summary?tag={tag_type}`    
    - mandatory : [`tag`](https://github.com/ohenley/ib-ada/blob/main/src/ib_ada.ads#L214-L224)
    - e.g. : `http://127.0.0.1:8080/accounts_summary?tag=NET_LIQUIDATION`

- positions (with profits) :
    - description : retrieves your current positions (trades) data. 
    - pattern : `http://{base_url}/positions?profit_and_loss={boolean}`    
    - optional : `profit_and_loss`
    - e.g. : `http://127.0.0.1:8080/positions?profit_and_loss=true`

- commission :
    - description : retrieves how much it would cost to enter a position (trade).
    - pattern : `http://{base_url}/commission?side={order_side_type}&symbol={string}&quantity={integer}&at_price_type={order_at_price_type}`    
    - mandatory : [`side`](https://github.com/ohenley/ib-ada/blob/main/src/ib_ada.ads#L77)
    - mandatory : `symbol`
    - mandatory : `quantity`
    - mandatory : [`at_price_type`](https://github.com/ohenley/ib-ada/blob/main/src/ib_ada.ads#L78) 
    - e.g. : `http://127.0.0.1:8080/commission?side=BUY&symbol=IBM&quantity=10&at_price_type=MKT`

- place orders :
    - description : enter a trade for a given security.
    - pattern : `http://{base_url}/place_order?side={order_side_type}&symbol={string}&quantity={integer}&at_price_type={order_at_price_type}`    
    - mandatory : [`side`](https://github.com/ohenley/ib-ada/blob/main/src/ib_ada.ads#L77)
    - mandatory : `symbol`
    - mandatory : `quantity`
    - mandatory : [`at_price_type`](https://github.com/ohenley/ib-ada/blob/main/src/ib_ada.ads#L78) 
    - e.g. `http://127.0.0.1:8080/place_order?side=BUY&symbol=IBM&quantity=10&at_price_type=MIDPRICE`
    - note: in theory should be a POST call (todo) but it changes nothing in reality, there is no GET/POST police yet.

- cancel_orders :
    - description : cancel a trade already ordered to IB.
    - pattern : `http://{base_url}/cancel_order?request_id={integer}`
    - mandatory : `request_id`
    - e.g. `http://127.0.0.1:8080/cancel_order?request_id=871`
    - note: in theory should be a POST call (todo) but it changes nothing in reality, there is no GET/POST police yet.

- open orders :
    - description : retrieves trades already ordered to IB but not yet sealed/completed.
    - pattern : `http://{base_url}/open_orders`
    - e.g. `http://127.0.0.1:8080/open_orders`

## Acknowledgments
- Thanks to late @sparre for his wonderful and sane work on [black](https://github.com/sparre/black) which made the REST layer a breeze to make.
