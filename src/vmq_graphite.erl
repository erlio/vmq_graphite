%% Copyright 2014 Erlio GmbH Basel Switzerland (http://erl.io)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(vmq_graphite).
-export([start/0,
         stop/0,
         change_config/1]).

-define(REPORTER, vmq_report_graphite).

start() ->
    {ok, _} = application:ensure_all_started(vmq_graphite),
    vmq_graphite_cli:register(),
    ok.

stop() ->
    application:stop(vmq_graphite).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Hooks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
change_config(Config) ->
    case lists:keyfind(vmq_graphite, 1, application:which_applications()) of
        false ->
            %% vmq_systree app is loaded but not started
            ok;
        _ ->
            %% vmq_graphite app is started
            {vmq_graphite, GraphiteConfig} = lists:keyfind(vmq_graphite, 1, Config),
            Interval = proplists:get_value(graphite_interval, GraphiteConfig),
            ConnectTimeout = proplists:get_value(graphite_connect_timeout, GraphiteConfig),
            Prefix = proplists:get_value(graphite_prefix, GraphiteConfig),
            Port = proplists:get_value(graphite_port, GraphiteConfig),
            Host = proplists:get_value(graphite_host, GraphiteConfig),
            ApiKey = proplists:get_value(graphite_api_key, GraphiteConfig),
            exometer_report:disable_reporter(?REPORTER),
            exometer_report:remove_reporter(?REPORTER),
            Opts = [{api_key, ApiKey},
                    {prefix, Prefix},
                    {host, Host},
                    {port, Port},
                    {connect_timeout, ConnectTimeout}],
            exometer_report:add_reporter(?REPORTER, Opts),
            %% TODO: kind of a hack
            {ok, {apply, M, F, A}} = application:get_env(vmq_server,
                                                         exometer_predefined),
            {ok, Entries} = apply(M, F, A),
            ok = subscribe(Entries, Interval),
            exometer_report:enable_reporter(?REPORTER)
    end.

subscribe([{Metric, histogram, _}|Rest], Interval) ->
    exometer_report:subscribe(?REPORTER, Metric, value, Interval),
    exometer_report:subscribe(?REPORTER, Metric, max, Interval),
    exometer_report:subscribe(?REPORTER, Metric, min, Interval),
    exometer_report:subscribe(?REPORTER, Metric, mean, Interval),
    exometer_report:subscribe(?REPORTER, Metric, median, Interval),
    subscribe(Rest, Interval);
subscribe([{Metric, _, _}|Rest], Interval) ->
    exometer_report:subscribe(?REPORTER, Metric, value, Interval),
    subscribe(Rest, Interval);
subscribe([], _) -> ok.
