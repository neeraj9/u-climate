# U-Climate

[![Build Status](https://travis-ci.org/neeraj9/u-climate.svg?branch=master)](https://travis-ci.org/neeraj9/u-climate)
[![Software License (3-Clause BSD)](https://img.shields.io/badge/license-BSD%203--Clause-blue.svg?style=flat-square)](http://opensource.org/licenses/BSD-3-Clause)

This project hosts an Erlang Climate service, which exposes api for querying
climate information around the world. This is targeted towards the
microservices architecture for polling for climate or weather information.
This project is one of the service running Erlang microkernel powered
by the Rumprun unikernel.

The project is under active development and hence still very early stage
and not completely tested.

## Dependencies and Development Environment

This is documented in detail at
[hello-erlang-rump](https://github.com/neeraj9/hello-erlang-rump/blob/master/readme.md)

## Motivation

This service provides HTTP/2.0 API for querying world climate information.
The following list of sources exist in the current codebase:

* [openweathermap](http://openweathermap.org/)

## Build

After you are done setting up the development environment the build is
pretty straight-forward (see below).

    git clone https://github.com/neeraj9/u-climate
    cd u-climate
    make

## Thanks

Thanks for evaluating this project and hope you find it useful.
Feel free to create issues for bugs or new features.

## Authors

* Neeraj Sharma {[github: neeraj9](https://github.com/neeraj9)}
