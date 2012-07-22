package com.wheaties.squealer.generator

trait Formato{
  def databaseName(name: String):String
  def tableName(name: String):String
  def columnName(name: String):String
}