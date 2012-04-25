package com.wheaties.squealer

import annotation.tailrec

//TODO: new name, it ain't validation, per se
object CoalesceSQL{
  def apply(source: Database, sql: Expr) = sql match{
    case x:SQL =>
    case x:Union =>
    case _ =>
  }

  protected[squealer] def mapSQL(source: Database, sql: SQL) ={
    val SQL(selectClause, fromClause, whereClause) = sql
    val tables = CoalesceFromClause(fromClause, source)

  }

  def populateColumns(select: Select, tables: List[Table]) = select.terms flatMap{
    _ match{
      case Wildcard => tables.flatMap(_.columns)
      case count @ Count(_, _) => List(ColumnDef(termName(count), "Int", None, None))
      case term @ Term(_, _) => find(term, tables)
      case _ => List.empty[Column] //how'd this happen!?
    }
  }

  //TODO: figure out if can't be found. Perhaps should be Either[Exception,List]
  protected def find(term: Term, tables: List[Table]):List[Column] = term.term.split('.') match{
    case Array(front, "*") => tables find(_.name eq front) map(_.columns) getOrElse(List.empty[Column])
    case Array(front, back) =>
      val column = tables.find(_.name eq front) flatMap(_.columns.find(_.name eq termName(term)))
      column map(col => ReformatColumn(col, _ => termName(term))) toList
    case Array(name) =>
      val column = tables.flatMap(_.columns.find(_.name eq name)).headOption
      column map(col => ReformatColumn(col, _ => termName(term))) toList
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

object CoalesceFromClause extends ((From, Database) => List[Either[Exception,Table]]){

  def apply(fromClause: From, source: Database) ={
    val tableAliases = mapTableAliases(fromClause, source)

    @tailrec def transform(remainder:List[Expr], acc:List[Selection] = Nil):List[Selection] = remainder match{
      case Term(name, alias) :: xs =>
        val table = find(termName(name, alias), tableAliases)
        transform(remainder, table :: acc)
      case Join(_, Conditional(left, right)) :: xs =>
        val leftTable = find(left, tableAliases)
        val rightTable = findTable(right, acc)
        val tables = substitute(rightTable, acc)
        transform(xs, leftTable :: tables)
      case LeftJoin(_, Conditional(left, right)) :: xs =>
        val leftTable = find(left, tableAliases)
        val rightTable = findTable(right, acc, nullColumns(_))
        val tables = substitute(rightTable, acc)
        transform(xs, leftTable :: tables)
      case RightJoin(_, Conditional(left, right)) :: xs =>
        val leftTable = find(left, tableAliases, nullColumns(_))
        val rightTable = findTable(right, acc)
        val tables = substitute(rightTable, acc)
        transform(xs, leftTable :: tables)
      case FullJoin(_, Conditional(left, right)) :: xs =>
        val leftTable = find(left, tableAliases, nullColumns(_))
        val rightTable = findTable(right, acc, nullColumns(_))
        val tables = substitute(rightTable, acc)
        transform(xs, leftTable :: tables)
      case _ :: xs => transform(xs, acc)
      case Nil => acc
    }

    transform(fromClause.clauses)
  }

  /**
   * TODO:
   * 2. check that both columns are present in the join
   * 3. check that they're of compatible types
   * 4. throw an error if they're not that's informative and traceable
   */
  type Selection = Either[Exception,Table]

  class UnfoundTableError(name: String) extends Exception

  private[squealer] def findTable(term: String, tables: List[Selection], op: Table => Table = identity):Selection ={
    val name = termName(term, None)

    @tailrec def findTable(items: List[Selection]):Selection = items match{
      case Right(table) :: xs if table.name eq name => Right(op(table))
      case x :: xs => findTable(xs)
      case Nil => Left(new UnfoundTableError(term))
    }

    findTable(tables)
  }

  private[squealer] def find(name: String, tables: List[Table], op: Table => Table = identity):Selection ={
    @tailrec def findTable(items: List[Table]):Selection = items match{
      case table :: xs if table.name eq name => Right(op(table))
      case x :: xs => findTable(xs)
      case Nil => Left(new UnfoundTableError(name))
    }

    findTable(tables)
  }

  private[squealer] def substitute(selection: Selection, tables: List[Selection]) = selection match{
    case Right(table) =>
      @tailrec def subTables(input: List[Selection], acc: List[Selection] = Nil):List[Selection] = input match{
        case Right(Table(name, _, _)) :: xs if name == table.name => acc ::: Right(table) :: xs
        case x :: xs => subTables(xs, x :: acc)
        case Nil => acc
      }
      subTables(tables)
    case _ => selection :: tables
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

  protected[squealer]def mapTableAliases(fromClause: From, source: Database) = {
    @tailrec def terms(remainder: List[Expr], acc: List[Term] = Nil):List[Term] = remainder match{
      case Term(name, alias) :: xs => terms(xs, Term(name, alias) :: acc)
      case Join(term @ Term(_,_), _) :: xs => terms(xs, term :: acc)
      case LeftJoin(term @ Term(_,_), _) :: xs => terms(xs, term :: acc)
      case RightJoin(term @ Term(_,_), _) :: xs => terms(xs, term :: acc)
      case FullJoin(term @ Term(_,_), _) :: xs => terms(xs, term :: acc)
      case x :: xs => terms(xs, acc)
      case Nil => acc
    }

    for{
      term <- terms(fromClause.clauses)
      table <- source.tables.find(_.name == term.term)
    } yield table.copy(name = termName(term))
  }

  protected def termName(term: Term):String = termName(term.term, term.alias)
  protected def termName(term: String, alias: Option[String]) = alias.getOrElse{
    term.split(".") match{
      case Array(front, back) => back
      case Array(single) => single
    }
  }
}