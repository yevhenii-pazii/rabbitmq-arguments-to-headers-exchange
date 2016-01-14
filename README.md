# Arguments to headers Exchange [![Build Status](https://travis-ci.org/thecederick/rabbitmq-arguments-to-headers-exchange.svg?branch=master)] (https://travis-ci.org/thecederick/rabbitmq-arguments-to-headers-exchange)

This is a custom exchange for [RabbitMQ](http://www.rabbitmq.com/). Exchange has the same routing behavior as original **fanout** exchange with additional feature.

## Features

All arguments defined for exchange will be added to message headers or updated if such header exists.

## Usage Examples

Download released artifact:
```sh
wget https://github.com/thecederick/rabbitmq-arguments-to-headers-exchange/releases/download/rabbitmq-3.6.0-1.0.2/rabbitmq_arguments_to_headers_exchange-3.6.0.ez
```
Copy plugin to RabbitMQ plugins directory:
```sh
cp rabbitmq_arguments_to_headers_exchange-3.6.0.ez /usr/lib/rabbitmq/lib/rabbitmq_server-3.6.0/plugins
```
Enable plugin:
```sh
rabbitmq-plugins enable rabbitmq_arguments_to_headers_exchange
```
Create Exchange with type `argument`.


