-module(prometheus_buckets_tests).

-include_lib("eunit/include/eunit.hrl").

linear_errors_test() ->
    ?assertError(
        {invalid_value, 0, "Buckets count should be positive"},
        prometheus_buckets:linear(-15, 5, 0)
    ).

linear_test() ->
    ?assertEqual([-15, -10, -5, 0, 5, 10], prometheus_buckets:linear(-15, 5, 6)),
    ?assertEqual([1, 5.5, 10, 14.5, 19, 23.5], prometheus_buckets:linear(1, 4.5, 6)).

ddsketch_errors_test() ->
    ?assertError(
        {invalid_value, 5, "Buckets error should be a valid percentage point"},
        prometheus_buckets:ddsketch(5, 0)
    ),
    ?assertError(
        {invalid_value, 0, "Buckets count should be positive"},
        prometheus_buckets:ddsketch(0.01, 0)
    ).

ddsketch_test() ->
    ?assertEqual(
        [
            1.0,
            1.02020202020202,
            1.040812162024283,
            1.0618386703480058,
            1.0832899566176624,
            1.105174602205898,
            1.127501361846421,
            1.1502791671362476,
            1.1735171301086968,
            1.1972245468785696,
            1.2214109013609646
        ],
        prometheus_buckets:ddsketch(0.01, 10)
    ),
    ?assertEqual(
        [
            1.0,
            1.2222222222222223,
            1.4938271604938274,
            1.825788751714678,
            2.231519585429051,
            2.7274128266355073,
            3.3335045658878424,
            4.074283358307364,
            4.979679660153445,
            6.086275140187544,
            7.438780726895887,
            9.09184311065053,
            11.112252690795092,
            13.581642177638448,
            16.599784883780327,
            20.288625969064846,
            24.797209517745923,
            30.307700521689465,
            37.042745082064904,
            45.274466211412665,
            55.335458702837705
        ],
        prometheus_buckets:ddsketch(0.1, 20)
    ).

exponential_errors_test() ->
    ?assertError(
        {invalid_value, 0, "Buckets count should be positive"},
        prometheus_buckets:exponential(-15, 5, 0)
    ),
    ?assertError(
        {invalid_value, -15, "Buckets start should be positive"},
        prometheus_buckets:exponential(-15, 5, 2)
    ),
    ?assertError(
        {invalid_value, 0.5, "Buckets factor should be greater than 1"},
        prometheus_buckets:exponential(15, 0.5, 3)
    ).

exponential_test() ->
    ?assertEqual([100, 120, 144], prometheus_buckets:exponential(100, 1.2, 3)),
    ?assertEqual([1.5, 2.25, 3.375], prometheus_buckets:exponential(1.5, 1.5, 3)).

default_test() ->
    ?assertEqual(
        [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10],
        prometheus_buckets:default()
    ).

constructor_errors_test() ->
    ?assertError(
        {no_buckets, []},
        prometheus_buckets:new([])
    ),
    ?assertError(
        {no_buckets, undefined},
        prometheus_buckets:new(undefined)
    ),
    ?assertError(
        {invalid_buckets, 1, "not a list"},
        prometheus_buckets:new(1)
    ),
    ?assertError(
        {invalid_bound, "qwe"},
        prometheus_buckets:new(["qwe"])
    ),
    ?assertError(
        {invalid_buckets, [1, 3, 2], "buckets not sorted"},
        prometheus_buckets:new([1, 3, 2])
    ).

constructor_test() ->
    ?assertEqual(
        prometheus_buckets:default() ++ [infinity],
        prometheus_buckets:new()
    ),
    ?assertEqual(
        prometheus_buckets:default() ++ [infinity],
        prometheus_buckets:new(default)
    ),
    ?assertEqual(
        [-15, -10, -5, 0, 5, 10, infinity],
        prometheus_buckets:new({linear, -15, 5, 6})
    ),
    ?assertEqual(
        [100, 120, 144, infinity],
        prometheus_buckets:new({exponential, 100, 1.2, 3})
    ),
    ?assertEqual(
        [100, 300, 500, 750, 1000, infinity],
        prometheus_buckets:new([100, 300, 500, 750, 1000])
    ).
