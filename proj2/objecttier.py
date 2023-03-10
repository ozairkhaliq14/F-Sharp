#
# File: objecttier.py
#
# Builds Movie-related objects from data retrieved through
# the data tier.
#
# Original author:
#   Prof. Joe Hummel
#   U. of Illinois, Chicago
#   CS 341, Spring 2022
#   Project #02
#
import datatier


##################################################################
#
# Movie:
#
# Constructor(...)
# Properties:
#   Movie_ID: int
#   Title: string
#   Release_Year: string
#
class Movie:

    def __init__(self, mid=0, title="", year=""):
        self._Movie_ID = mid
        self._Title = title
        self._Release_Year = year

    @property
    def Movie_ID(self):
        return self._Movie_ID

    @property
    def Title(self):
        return self._Title

    @property
    def Release_Year(self):
        return self._Release_Year


##################################################################
#
# MovieRating:
#
# Constructor(...)
# Properties:
#   Movie_ID: int
#   Title: string
#   Release_Year: string
#   Num_Reviews: int
#   Avg_Rating: float
#
class MovieRating:

    def __init__(self, mid=0, title="", year="", reviews=0, rating=0.0):
        self._Movie_ID = mid
        self._Title = title
        self._Release_Year = year
        self._Num_Reviews = reviews
        self._Avg_Rating = rating

    @property
    def Movie_ID(self):
        return self._Movie_ID

    @property
    def Title(self):
        return self._Title

    @property
    def Release_Year(self):
        return self._Release_Year

    @property
    def Num_Reviews(self):
        return self._Num_Reviews

    @property
    def Avg_Rating(self):
        return self._Avg_Rating


##################################################################
#
# MovieDetails:
#
# Constructor(...)
# Properties:
#   Movie_ID: int
#   Title: string
#   Release_Date: string, date only (no time)
#   Runtime: int (minutes)
#   Original_Language: string
#   Budget: int (USD)
#   Revenue: int (USD)
#   Num_Reviews: int
#   Avg_Rating: float
#   Tagline: string
#   Genres: list of string
#   Production_Companies: list of string
#
class MovieDetails:

    def __init__(self,
                 mid=0,
                 title="",
                 release="",
                 runtime=0,
                 lang="",
                 budget=0,
                 revenue=0,
                 reviews=0,
                 rating=0.0,
                 tagline="",
                 genres=[],
                 companies=[]):
        self._Movie_ID = mid
        self._Title = title
        self._Release_Date = release
        self._Runtime = runtime
        self._Original_Language = lang
        self._Budget = budget
        self._Revenue = revenue
        self._Num_Reviews = reviews
        self._Avg_Rating = rating
        self._Tagline = tagline
        self._Genres = genres
        self._Production_Companies = companies

    @property
    def Movie_ID(self):
        return self._Movie_ID

    @property
    def Title(self):
        return self._Title

    @property
    def Release_Date(self):
        return self._Release_Date

    @property
    def Runtime(self):
        return self._Runtime

    @property
    def Original_Language(self):
        return self._Original_Language

    @property
    def Budget(self):
        return self._Budget

    @property
    def Revenue(self):
        return self._Revenue

    @property
    def Num_Reviews(self):
        return self._Num_Reviews

    @property
    def Avg_Rating(self):
        return self._Avg_Rating

    @property
    def Tagline(self):
        return self._Tagline

    @property
    def Genres(self):
        return self._Genres

    @property
    def Production_Companies(self):
        return self._Production_Companies


##################################################################
#
# num_movies:
#
# Returns: # of movies in the database; if an error returns -1
#
def num_movies(dbConn):

    sql = """SELECT count (*) FROM Movies;"""

    row = datatier.select_one_row(dbConn, sql)

    if row is None:
        return -1
    return row[0]


##################################################################
#
# num_reviews:
#
# Returns: # of reviews in the database; if an error returns -1
#
def num_reviews(dbConn):

    sql = """SELECT count (*) FROM Ratings;"""

    row = datatier.select_one_row(dbConn, sql)

    if row is None:
        return -1
    return row[0]


##################################################################
#
# get_movies:
#
# gets and returns all movies whose name are "like"
# the pattern. Patterns are based on SQL, which allow
# the _ and % wildcards. Pass "%" to get all stations.
#
# Returns: list of movies in ascending order by name;
#          an empty list means the query did not retrieve
#          any data (or an internal error occurred, in
#          which case an error msg is already output).
#
def get_movies(dbConn, pattern):

    sql = """SELECT Movie_ID, Title, strftime('%Y', Release_Date) FROM Movies WHERE Title LIKE ? ORDER BY Title asc;"""

    rows = datatier.select_n_rows(dbConn, sql, [pattern])

    if rows is None:
        return []

    moviesList = []
    for row in rows:
        i = Movie(row[0], row[1], row[2])
        moviesList.append(i)

    return moviesList


##################################################################
#
# get_movie_details:
#
# gets and returns details about the given movie; you pass
# the movie id, function returns a MovieDetails object. Returns
# None if no movie was found with this id.
#
# Returns: if the search was successful, a MovieDetails obj
#          is returned. If the search did not find a matching
#          movie, None is returned; note that None is also
#          returned if an internal error occurred (in which
#          case an error msg is already output).
#
def get_movie_details(dbConn, movie_id):

    #movie
    sqlM = """SELECT Movie_ID, Title, strftime('%Y-%m-%d', Release_Date), Runtime, Original_Language, Budget, Revenue FROM Movies WHERE Movie_ID = ?;"""

    rowM = datatier.select_one_row(dbConn, sqlM, [movie_id])

    if rowM is None:
        return None
    if rowM == ():
        return None

    #rating

    sqlR = """SELECT count (*), avg(Rating) FROM Ratings WHERE Movie_ID = ?;"""

    rowR = datatier.select_one_row(dbConn, sqlR, [movie_id])
    reviews = rowR[0]

    if rowR[1] is None:
        rating = 0.0
    else:
        rating = rowR[1]

    #genres

    sqlG = """SELECT Genre_name FROM Genres JOIN Movie_Genres WHERE Genres.Genre_ID = Movie_Genres.Genre_ID AND Movie_Genres.Movie_ID = ? ORDER BY Genre_Name asc;"""

    rowG = datatier.select_n_rows(dbConn, sqlG, [movie_id])

    genreList = []
    for i in rowG:
        genreList.append(i[0])

    #companies

    sqlC = """SELECT Company_Name FROM Companies JOIN Movie_Production_Companies WHERE Companies.Company_ID = Movie_Production_Companies.Company_ID AND Movie_Production_Companies.Movie_ID = ? ORDER BY Company_Name asc;"""

    rowC = datatier.select_n_rows(dbConn, sqlC, [movie_id])

    companyList = []
    for i in rowC:
        companyList.append(i[0])

    #tagline

    sqlT = """SELECT Tagline FROM Movie_Taglines WHERE Movie_ID = ?;"""

    rowT = datatier.select_one_row(dbConn, sqlT, [movie_id])

    if rowT == ():
        line = ""
    else: 
        line = rowT[0]

    ret = MovieDetails(rowM[0], rowM[1], rowM[2], rowM[3], rowM[4], rowM[5],
                       rowM[6], reviews, rating, line, genreList,
                       companyList)

    return ret


##################################################################
#
# get_top_N_movies:
#
# gets and returns the top N movies based on their average
# rating, where each movie has at least the specified # of
# reviews. Example: pass (10, 100) to get the top 10 movies
# with at least 100 reviews.
#
# Returns: returns a list of 0 or more MovieRating objects;
#          the list could be empty if the min # of reviews
#          is too high. An empty list is also returned if
#          an internal error occurs (in which case an error
#          msg is already output).
#
def get_top_N_movies(dbConn, N, min_num_reviews):
    sql = """SELECT Movies.Movie_ID, Title, 
  strftime('%Y', Release_Date), count(Rating), avg(Rating) FROM Movies JOIN Ratings WHERE Ratings.Movie_ID=Movies.Movie_ID GROUP BY Movies.Movie_ID HAVING count(Rating) >= ? ORDER BY avg(Rating) desc LIMIT ?;"""
    rows = datatier.select_n_rows(dbConn, sql, [min_num_reviews, N])

    if rows is None:
        return []

    ratingsList = []
    for i in rows:
        j = MovieRating(i[0], i[1], i[2], i[3], i[4])
        ratingsList.append(j)

    return ratingsList


##################################################################
#
# add_review:
#
# Inserts the given review --- a rating value 0..10 --- into
# the database for the given movie. It is considered an error
# if the movie does not exist (see below), and the review is
# not inserted.
#
# Returns: 1 if the review was successfully added, returns
#          0 if not (e.g. if the movie does not exist, or if
#          an internal error occurred).
#
def add_review(dbConn, movie_id, rating):
    sql = """SELECT Movie_ID FROM Movies WHERE Movie_ID = ?;"""

    row = datatier.select_one_row(dbConn, sql, [movie_id])

    if row is None:
        return 0
    if row == ():
        return 0

    sqlR = """INSERT INTO Ratings VALUES (?, ?);"""

    rowR = datatier.perform_action(dbConn, sqlR, [movie_id, rating])

    if rowR == -1:
        return 0
    else:
        return 1


##################################################################
#
# set_tagline:
#
# Sets the tagline --- summary --- for the given movie. If
# the movie already has a tagline, it will be replaced by
# this new value. Passing a tagline of "" effectively
# deletes the existing tagline. It is considered an error
# if the movie does not exist (see below), and the tagline
# is not set.
#
# Returns: 1 if the tagline was successfully set, returns
#          0 if not (e.g. if the movie does not exist, or if
#          an internal error occurred).
#
def set_tagline(dbConn, movie_id, tagline):
    sql = """SELECT Movie_ID FROM Movies Where Movie_ID = ?;"""
    row = datatier.select_one_row(dbConn, sql, [movie_id])

    if row is None or row == ():
        return 0

    sqlT = """SELECT Movie_ID FROM Movie_Taglines WHERE Movie_ID = ?;"""
    rowT = datatier.select_one_row(dbConn, sqlT, [movie_id])

    if rowT != ():
        updateDB = """UPDATE Movie_Taglines SET Tagline = ? WHERE Movie_ID = ?;"""
        newRow = datatier.perform_action(dbConn, updateDB, [tagline, movie_id])
    else:
        insDB = """INSERT INTO Movie_Taglines VALUES (?, ?);"""
        newRow = datatier.perform_action(dbConn, insDB, [movie_id, tagline])

    if newRow == -1:
        return 0
    else:
        return 1
