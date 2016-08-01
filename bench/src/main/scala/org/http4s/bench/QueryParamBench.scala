package org.http4s.bench

import org.http4s.Query
import org.http4s.bench.input.QueryParamInput
import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
class QueryParamBench {

  @Benchmark def withQueryParam(in: QueryParamInput): Query =
    in.queryParams.foldLeft(Query.empty){ case (query, (key, value)) => query.withQueryParam(key, value) }

  @Benchmark def setQueryParams(in: QueryParamInput): Query =
    Query.empty.setQueryParams(in.queryParams.mapValues(Seq(_)))

}
