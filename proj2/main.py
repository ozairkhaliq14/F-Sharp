#
# Name: Ozair Khaiq
# Course: CS 341
# Professor Patrick Troy
# Project 2: MovieLens application
# Due Date: 10/7/2022
#

import sqlite3
import objecttier

dbConn = sqlite3.connect('MovieLens.db')

cmd = ''
print("** Welcome to the MovieLens app **")
print("\nGeneral stats:")
print(f"  # of movies:{objecttier.num_movies(dbConn): ,}")
print(f"  # of reviews:{objecttier.num_reviews(dbConn): ,}")


# command 1: Inputs a movie name and outputs the movie’s ID, title, and year of release.
def cmd1(dbConn):
    print("Enter movie name (wildcards _ and % supported): ", end='')
    cmd = input()
    list = objecttier.get_movies(dbConn, cmd)
    listLen = len(list)
    print(f"\n# of movies found: {listLen}\n")

    if (listLen > 100):
        print(
            "There are too many movies to display, please narrow your search and try again...\n"
        )
        return None
    for i in list:
        print(f"{i.Movie_ID} : {i.Title} ({i.Release_Year})")


# command 2: Inputs a movie id and outputs detailed movie information about this movie --- tagline, budget, revenue, genres, etc.
def cmd2(dbConn):
    print("Enter movie id: ", end='')
    cmd = input()
    list = objecttier.get_movie_details(dbConn, cmd)

    if (list is None):
        print("\nNo such movie...")
        return 0
    else:
        print(f"\n{(list.Movie_ID)} : {list.Title}")
        print(f"  Release date: {list.Release_Date}")
        print(f"  Runtime: {(list.Runtime)} (mins)")
        print(f"  Orig language: {list.Original_Language}")
        print(f"  Budget: ${list.Budget:,} (USD)")
        print(f"  Revenue: ${list.Revenue:,} (USD)")
        print(f"  Num reviews: {(list.Num_Reviews)}")
        print(f"  Avg rating: {list.Avg_Rating:.2f} (0..10)")

        print("  Genres: ", end='')
        for i in list.Genres:
            print(f"{i}, ", end='')

        print("\n  Production companies: ", end='')
        for i in list.Production_Companies:
            print(f"{i}, ", end='')

        print(f"\n  Tagline: {list.Tagline}")


# command 3: Output the top N movies based on their average rating *and* with a minimum number of reviews.
def cmd3(dbConn):
    print("N? ", end='')
    cmd = int(input())
    if (cmd <= 0):
        print("Please enter a positive value for N...\n")
        return 0

    print("min number of reviews? ", end='')
    cmdTwo = int(input())
    if (cmdTwo <= 0):
        print("Please enter a positive value for min number of reviews...")
        return 0

    list = objecttier.get_top_N_movies(dbConn, cmd, cmdTwo)
    print()

    for i in list:
        print(
            f"{(i.Movie_ID)} : {i.Title} ({i.Release_Year}), avg rating = {i.Avg_Rating:.2f} ({i.Num_Reviews} reviews)"
        )


# command 4: Inserts a new review into the database
def cmd4(dbConn):
    print("Enter rating (0..10): ", end='')
    cmd = int(input())
    if (cmd < 0 or cmd > 10):
        print("Invalid rating...\n")
        return 0
    else:
        print("Enter movie id: ", end='')
        cmdTwo = int(input())

        insert = objecttier.add_review(dbConn, cmdTwo, cmd)

        if (insert != 0):
            print("\nReview successfully inserted\n")
        else:
            print("\nNo such movie...\n")


# command 5: Sets the tagline for a given movie, either by inserting (if not already there) or updating (if already there).
def cmd5(dbConn):
    print("tagline? ", end='')
    cmd = input()
    print("movie id? ", end='')
    cmdTwo = input()

    insert = objecttier.set_tagline(dbConn, cmdTwo, cmd)

    if (insert != 0):
        print("\nTagline successfully set")
    else:
        print("\nNo such movie...")


while cmd != 'x':
    print("\nPlease enter a command (1-5, x to exit): ", end='')
    print()
    cmd = input()

    if (cmd == '1'):
        cmd1(dbConn)
    elif (cmd == '2'):
        cmd2(dbConn)
    elif (cmd == '3'):
        cmd3(dbConn)
    elif (cmd == '4'):
        cmd4(dbConn)
    elif (cmd == '5'):
        cmd5(dbConn)
    else:
        break
