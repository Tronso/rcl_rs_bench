rcl_rs_bench
=====

Benchmarking tool for [riak_core_lite](https://github.com/riak-core-lite/riak_core_lite) using [rcl_ref](https://github.com/wattlebirdaz/rclref) as the reference key-value store implementation.

Benchmarking
-----

1. Adapt `src/rcl_rs_driver.erl`:
    1. Change the output of `concurrent_workers/0` to the number of nodes in the cluster that should execute operations.
    2. If the naming-convention of the nodes in the cluster varies from `dev<n>@127.0.0.1` you have to adapt ``new/1` to create the correct node names for the workers.
    3. Set the duration of the benchmark by adapting the output of `duration/0`.
2. Start the cluster of `rcl_ref`-nodes and join them.
3. Execute the benchmark with
   ```
   $ make run
   ```

4. After the benchmark has ended, generate a cummulative data log and a visualization of latency and throughput with
   ```
   $ make visualize
   ```
   The output can be found in the `results` directory. For the detailed logs per operation see `_build/default/rel/rcl_rs_bench/tests`.
