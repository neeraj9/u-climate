%%%-------------------------------------------------------------------
%%% @author nsharma
%%% @copyright (C) 2016, Neeraj Sharma
%%% @doc
%%%
%%% @end
%%% Copyright (c) 2016, Neeraj Sharma <neeraj.sharma@alumni.iitg.ernet.in>
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%
%%% * Redistributions of source code must retain the above copyright notice, this
%%%   list of conditions and the following disclaimer.
%%%
%%% * Redistributions in binary form must reproduce the above copyright notice,
%%%   this list of conditions and the following disclaimer in the documentation
%%%   and/or other materials provided with the distribution.
%%%
%%% * Neither the name of u-climate nor the names of its
%%%   contributors may be used to endorse or promote products derived from
%%%   this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
%%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%%% OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------
-module(uclimate_geocircle_handler).
-author("nsharma").

%% API

%% Standard callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).

%% Custom callbacks
-export([json_text/2]).

-define(DEFAULT_MAXIMUM_CITIES_BIN, <<"10">>).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, json_text}
  ], Req, State}.

%% Need latitude and longitude as query string within the
%% request otherwise this API will fail and the process will
%% crash, so cowboy will return HTTP/1.1 500 Internal Server Error.
%% TODO the HTTP 500 error code is inappropriate for errors
%% where the user provided incorrect URL or missing options.
%% Instead return bad request http code.
-spec(json_text(Req :: term(), State :: term()) ->
  {ResponseBody :: string(), Req :: term(), State :: term()}).
json_text(Req, State) ->
  #{lat:=LatVal, lon:=LongVal, cnt:=CntVal} =
    cowboy_req:match_qs([lat, lon, {cnt, [], ?DEFAULT_MAXIMUM_CITIES_BIN}],
      Req),
  % lets try to convert them for validation rather than pass
  % to third-party site as-is and fail there unnecessarily.
  GeoLat = safe_binary_to_float(LatVal),
  GeoLong = safe_binary_to_float(LongVal),
  CityCount = binary_to_integer(CntVal),
  Response = gen_server:call(
    openweather_proxy, {geocircle, GeoLat, GeoLong, CityCount}),
  case Response of
    {ok, Value} ->
      ResponseBody = Value;
    {error, _} ->
      ResponseBody = <<"{}">>
  end,
  {ResponseBody, Req, State}.

safe_binary_to_float(Bin) ->
  NumString = binary_to_list(Bin),
  case string:to_float(NumString) of
    {error, no_float} ->
      case string:to_integer(NumString) of
        {error, no_integer} ->
          error;
        % convert integer to float
        {IntVal, _Rest} -> IntVal / 1.0
      end;
    {Val, _Rest} -> Val
  end.