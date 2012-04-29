package com.wheaties.squealer

import annotation.tailrec

object CoalesceSQL{
  def apply(source: Database, sql: Expr) = sql match{
    case x:SQL =>
    case x:Union =>
    case _ =>
  }

  protected[squealer] def mapSQL(source: Database, sql: SQL) ={
    val SQL(selectClause, fromClause, whereClause) = sql
    val tables = CoalesceFromClause(fromClause, source.tables)
    val columns = CoalesceSelectClause(selectClause, tables)
  }
}

object CoalesceSelectClause extends ((Select, Map[Term,Table]) => List[Column]){

  def apply(select: Select, tableMap: Map[Term,Table]):List[Column] ={
    val columnMap = findColumn(tableMap)

    //TODO: capture missing!
    val columns = select.terms flatMap{
      _ match{
        case Wildcard => tableMap.values.flatMap(_.columns).toList
        case phrase:Count if columnMap(phrase.term).isDefined => List(ColumnDef(termName(phrase), "Int", None, None))
        case phrase:Term => columnMap(phrase.term)
        case _ => List.empty[Column] //how'd this happen!?
      }
    }

    columns
  }

  protected[squealer] def findColumn(tableMap: Map[Term,Table])={
    val tableNameMap = for{
      (term,table) <- tableMap
      tableName <- termName(term) :: term.term :: Nil
    } yield (tableName, table)
    val columnNameMap = for{
      (_, table) <- tableMap
      column <- table.columns
    } yield (column.name, column)

    def column(phrase: String):Option[Column]={
      termTable(phrase) match{
        case ("", columnName) => columnNameMap.get(columnName)
        case (tableName, columnName) => for{
          table <- tableNameMap.get(tableName)
          column <- table.columns.find(_.name eq columnName)
        } yield column
      }
    }

    column _
  }

  protected[squealer] def termTable(name: String):(String,String) = name.split(".") match{
    case Array(front, back) => (front,back)
    case Array(unique) => ("", unique)
  }

  //TODO: this is repeated, collapse into one entity in one place
  protected def termName(count: Count):String = termName(count.term, count.alias)
  protected def termName(term: Term):String = termName(term.term, term.alias)
  protected def termName(term: String, alias: Option[String]) = alias.getOrElse{
    term.split(".") match{
      case Array(front, back) => back
      case Array(single) => single
    }
  }
}

//TODO: capture errors!
object CoalesceFromClause extends ((From, List[Table]) => Map[Term,Table]){

  def apply(fromClause: From, tables: List[Table]) ={
    val terms = extractTerms(fromClause.clauses)
    val exists = terms.flatMap(t => t :: termName(t) :: Nil).toSet.contains _

    @tailrec def transform(remainder:List[Expr], mapped:Map[Term,Table]):Map[Term,Table] = remainder match{
      case Term(name, alias) :: xs if exists(name) => transform(remainder, mapped)
      case Join(term, Conditional(left, right)) :: xs if exists(left) && exists(right) => transform(xs, mapped)
      case LeftJoin(_, Conditional(left, right)) :: xs if exists(left) && exists(right) =>
        val subbed = substitute(right, mapped, nullColumns)
        transform(xs, subbed)
      case RightJoin(_, Conditional(left, right)) :: xs if exists(left) && exists(right) =>
        val subbed = substitute(left, mapped, nullColumns)
        transform(xs, subbed)
      case FullJoin(_, Conditional(left, right)) :: xs if exists(left) && exists(right) =>
        val subbed = substitute(left, mapped, nullColumns)
        val resubbed = substitute(right, subbed, nullColumns)
        transform(xs, resubbed)
      case _ :: xs => transform(xs, mapped) //TODO: right here, need a fake "error Table" or "error term"
      case Nil => mapped
    }

    val tableAliases = mapTableAliases(terms, tables)
    transform(fromClause.clauses, tableAliases)
  }

  protected[squealer] def substitute(name: String, mapped: Map[Term,Table], op: Table => Table):Map[Term,Table] ={
    def matches(value: String)(term: Term) = (term.term eq value) || (term.alias.exists(_ eq value))
    val substituted = for{(term,table) <- mapped if matches(name)(term)} yield (term, op(table))
    mapped ++ substituted
  }

  protected[squealer] def nullColumns(table: Table)={
    val columns = table.columns map{col =>
      new NullableColumnDef(col.name, col.typeOf, col.comment){
        override def size: Int = col.size
        override def precision: Int = col.precision
        override def scale: Int = col.scale
        override def length: Int = col.length
      }
    }
    table.copy(columns = columns)
  }

  protected[squealer] def extractTerms(expressions: List[Expr]) = {
    @tailrec def terms(remainder: List[Expr], acc: List[Term] = Nil):List[Term] = remainder match{
      case Term(name, alias) :: xs => terms(xs, Term(name, alias) :: acc)
      case Join(term @ Term(_,_), _) :: xs => terms(xs, term :: acc)
      case LeftJoin(term @ Term(_,_), _) :: xs => terms(xs, term :: acc)
      case RightJoin(term @ Term(_,_), _) :: xs => terms(xs, term :: acc)
      case FullJoin(term @ Term(_,_), _) :: xs => terms(xs, term :: acc)
      case x :: xs => terms(xs, acc)
      case Nil => acc
    }

    terms(expressions)
  }

  protected[squealer] def mapTableAliases(terms: List[Term], tables: List[Table]) ={
    val paired = for{
      term <- terms
      table <- tables.find(_.name == term.term)
    } yield (term, table)

    paired.toMap
  }

  protected def termName(term: Term):String = termName(term.term, term.alias)
  protected def termName(term: String, alias: Option[String]) = alias.getOrElse{
    term.split(".") match{
      case Array(front, back) => back
      case Array(single) => single
    }
  }
}

class UnfoundTableError(name: String) extends Exception
class UnfoundColumnError(name: String) extends Exception