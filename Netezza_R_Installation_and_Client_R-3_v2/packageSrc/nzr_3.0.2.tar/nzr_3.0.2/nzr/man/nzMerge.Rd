% 
% Copyright (c) 2010, 2014, IBM Corp. All rights reserved. 
% 		
% This program is free software: you can redistribute it and/or modify 
% it under the terms of the GNU General Public License as published by 
% the Free Software Foundation, either version 3 of the License, or 
% (at your option) any later version. 
%
% This program is distributed in the hope that it will be useful, 
% but WITHOUT ANY WARRANTY; without even the implied warranty of 
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
% GNU General Public License for more details. 
%
% You should have received a copy of the GNU General Public License 
% along with this program. If not, see <http://www.gnu.org/licenses/>. 
% 
% 
\name{nzMerge}
\alias{nzMerge}
\title{Merge nz.data.frames}
\description{
  This function merges two \code{nz.data.frame} objects.
}
\usage{
nzMerge(x, y, by = intersect(x@cols, y@cols), by.x = by, by.y = by, 
    all = FALSE, all.x = all, all.y = all, sort = TRUE, 
    suffixes = c("_x", "_y"), table = NULL, on = NULL, type = NA)
}
\arguments{
  \item{x, y}{Objects of the \code{nz.data.frame} class}
  \item{by, by.x, by.y}{Specification of the common columns; see the
    \emph{Details} section.}
  \item{all}{Logical; \code{all = L} is shorthand for \code{all.x = L} and \code{all.y = L}. }
  \item{all.x}{Logical; if \code{TRUE}, then extra rows are added to the
    output, one for each row in \code{x} that has no matching row in
	\code{y}. These rows have a value of \code{NA} in those columns
	that are typically filled with values from \code{y}. The default is
	\code{FALSE}, so that only rows with data from both \code{x} and
	\code{y} are included in the output.}
  \item{all.y}{Logical; analogous to \code{all.x} above.}
  \item{sort}{Not used. Included for compatibility only.}
  \item{suffixes}{\code{character(2)} specifying the suffixes to be
    used for making non-\code{by names()} unique. Note that
	\code{nzMerge} uses the underscore character (_) for default suffixes
	rather than the dot (.), which is not a valid character for a column
	name.}
  \item{table}{Name of the output \code{nz.data.frame}.}
  \item{on}{Joining conditions.}
  \item{type}{Optional join type; if not provided, the system joins the
          tables based on the value of the ‘on’ parameter.}
}
\details{

If \code{on} is not \code{NULL}, the nzJoin function is called.

  The function allows you to join only two \code{nz.data.frame}s, which
  are passed as the first two arguments. The \code{ON} part of the
  \code{JOIN} clause can be specified in a number of ways:

\enumerate{
  \item by leaving default values of \code{on} and \code{type},
    which results in a \emph{NATURAL} join
  \item by setting \code{on} to \code{TRUE}, which results in a
    \code{CROSS} join
  \item by passing a character string as \code{on} and setting
    \code{type} to \code{'ON'}, which results in a regular join with
        the string cancatenated to the SQL query
  \item by passing a \code{nz.data.frame.join.cond} object
    as \code{on}, which results a regular join with the \code{ON}
        clause derived from the object
}

If \code{on} is not \code{NULL}, the nzMerge function is executed.

The \code{nz.data.frame.join.cond} object can be created with
overloaded operator methods (\code{<}, \code{>}, \code{<=}, \code{>=},
\code{==}, and \code{!=}) accepting two one-collumn \code{nz.data.frame}
objects as arguments. These can be combined with logical operators of
\code{|} and \code{&}.


By default the \code{nz.data.frame}s are merged on the columns common
to both data frames, however, but separate specifications of the
columns can be given by \code{by.x} and \code{by.y}.

Columns can be specified by name, number or by a logical vector: the
name \code{row.names} or the number \code{0} specifies the row names.
The rows in the two data frames that match on the specified columns
are extracted, and joined together. If there is more than one match,
all possible matches contribute one row each. For the precise meaning
of \sQuote{match}, see \code{\link{match}}. 

If \code{by} or both \code{by.x} and \code{by.y} are of length 0 (a
length zero vector or \code{NULL}), the result, \code{r}, is the
Cartesian product of \code{x} and \code{y}, that is, a \emph{cross join}.

If \code{all.x} is true, all the non matching cases of \code{x} are
also appended to the result, with a value of \code{NA} filled in the
corresponding columns of \code{y}; this works analogously for
\code{all.y}. 

If the remaining columns in the data frames have any common names,
these have suffixes, \code{_x} and \code{_y} by default, appended
to make the names of the result unique. Note that \code{nzMerge}
the underscore character (_) for default suffixes rather than the
dot (.), which is not a valid character for a column name.

In SQL database terminology, the default value of \code{all = FALSE}
is an inner join that is equivalent to a natural join. Specifying
\code{all.x = TRUE} gives a left (outer) join, \code{all.y = TRUE} a
right (outer) join, and both \code{all=TRUE} a (full) outer join.
}
\value{
  An \code{nz.data.frame}. The columns are the common columns followed
  by the remaining columns in \code{x} and then those in \code{y}. 
}
\seealso{\code{\link{merge}}, \code{\link{nz.data.frame}}}
\examples{
nzConnectDSN('NZSQL')

# setup example data
cows <- data.frame(CNUMBER = c(1, 2, 3), 
CBREED = c("Holstein", "Guernsey", "Angus"))
a <- as.nz.data.frame(cows, "cows_one", clear.existing=TRUE)
cows2 <- data.frame(CNUMBER = c(2, 3, 4), 
BREEDS = c('Jersey', 'Brown Swiss', 'Ayrshire'))
b <- as.nz.data.frame(cows2, "cows_two", clear.existing=TRUE)

# cross join (cartesian product)
c <- nzMerge(a, b, by=NULL)

# inner join on common columns (e.g. CNUMBER)
d <- nzMerge(a, b)

# inner join with merge columns explicitly stated
e <- nzMerge(a, b, by.x="CNUMBER", by.y="CNUMBER")

# left outer join on common columns (e.g. CNUMBER)
f <- nzMerge(a, b, all.x=TRUE)

# right outer join on common columns (e.g. CNUMBER)
g <- nzMerge(a, b, all.y=TRUE)

# full outer join on common columns (e.g. CNUMBER)
h <- nzMerge(a, b, all.x=TRUE, all.y=TRUE)

nzDisconnect()
}
