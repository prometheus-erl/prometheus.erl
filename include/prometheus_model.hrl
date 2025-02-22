%% -*- coding: utf-8 -*-
%% Automatically generated, do not edit
%% Generated by gpb_compile version 4.21.1

-ifndef(prometheus_model).
-define(prometheus_model, true).

-define(prometheus_model_gpb_version, "4.21.1").


-ifndef('LABELPAIR_PB_H').
-define('LABELPAIR_PB_H', true).
-record('LabelPair',
        {name                   :: unicode:chardata() | undefined, % = 1, optional
         value                  :: unicode:chardata() | undefined % = 2, optional
        }).
-endif.

-ifndef('GAUGE_PB_H').
-define('GAUGE_PB_H', true).
-record('Gauge',
        {value                  :: float() | integer() | infinity | '-infinity' | nan | undefined % = 1, optional
        }).
-endif.

-ifndef('COUNTER_PB_H').
-define('COUNTER_PB_H', true).
-record('Counter',
        {value                  :: float() | integer() | infinity | '-infinity' | nan | undefined % = 1, optional
        }).
-endif.

-ifndef('QUANTILE_PB_H').
-define('QUANTILE_PB_H', true).
-record('Quantile',
        {quantile               :: float() | integer() | infinity | '-infinity' | nan | undefined, % = 1, optional
         value                  :: float() | integer() | infinity | '-infinity' | nan | undefined % = 2, optional
        }).
-endif.

-ifndef('SUMMARY_PB_H').
-define('SUMMARY_PB_H', true).
-record('Summary',
        {sample_count           :: non_neg_integer() | undefined, % = 1, optional, 64 bits
         sample_sum             :: float() | integer() | infinity | '-infinity' | nan | undefined, % = 2, optional
         quantile = []          :: [prometheus_model:'Quantile'()] | undefined % = 3, repeated
        }).
-endif.

-ifndef('UNTYPED_PB_H').
-define('UNTYPED_PB_H', true).
-record('Untyped',
        {value                  :: float() | integer() | infinity | '-infinity' | nan | undefined % = 1, optional
        }).
-endif.

-ifndef('HISTOGRAM_PB_H').
-define('HISTOGRAM_PB_H', true).
-record('Histogram',
        {sample_count           :: non_neg_integer() | undefined, % = 1, optional, 64 bits
         sample_sum             :: float() | integer() | infinity | '-infinity' | nan | undefined, % = 2, optional
         bucket = []            :: [prometheus_model:'Bucket'()] | undefined % = 3, repeated
        }).
-endif.

-ifndef('BUCKET_PB_H').
-define('BUCKET_PB_H', true).
-record('Bucket',
        {cumulative_count       :: non_neg_integer() | undefined, % = 1, optional, 64 bits
         upper_bound            :: float() | integer() | infinity | '-infinity' | nan | undefined % = 2, optional
        }).
-endif.

-ifndef('METRIC_PB_H').
-define('METRIC_PB_H', true).
-record('Metric',
        {label = []             :: [prometheus_model:'LabelPair'()] | undefined, % = 1, repeated
         gauge                  :: prometheus_model:'Gauge'() | undefined, % = 2, optional
         counter                :: prometheus_model:'Counter'() | undefined, % = 3, optional
         summary                :: prometheus_model:'Summary'() | undefined, % = 4, optional
         untyped                :: prometheus_model:'Untyped'() | undefined, % = 5, optional
         histogram              :: prometheus_model:'Histogram'() | undefined, % = 7, optional
         timestamp_ms           :: integer() | undefined % = 6, optional, 64 bits
        }).
-endif.

-ifndef('METRICFAMILY_PB_H').
-define('METRICFAMILY_PB_H', true).
-record('MetricFamily',
        {name                   :: unicode:chardata() | undefined, % = 1, optional
         help                   :: unicode:chardata() | undefined, % = 2, optional
         type                   :: 'COUNTER' | 'GAUGE' | 'SUMMARY' | 'UNTYPED' | 'HISTOGRAM' | integer() | undefined, % = 3, optional, enum MetricType
         metric = []            :: [prometheus_model:'Metric'()] | undefined % = 4, repeated
        }).
-endif.

-endif.
