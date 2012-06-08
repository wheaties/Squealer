package com.wheaties.squealer.db

sealed trait ColumnType
case object ColumnDef extends ColumnType
case object NullableColumn extends ColumnType
case object PrimaryKey extends ColumnType
case object NullablePrimaryKey extends ColumnType