Einstein's Dreams, Daily
========================

This is the code for [this site](http://ed.jgl.me), which will post today's
dream from the book [Einstein's Dreams.](https://en.wikipedia.org/wiki/Einstein%27s_Dreams)

<u>Einstein's Dreams</u>, by Alan Lightman is one of my favorite books. A
brilliant mix of prose and poetry, it is a fictionalized account of Einstein's
life while he was figuring out the theory of relativity. It is told through a
series of dreams, each happening on a different night in 1905 during the months
of April, May, and June. In each dream, time works a little differently, and
Lightman explores the implications it has on humans and their societies.  This
site will post the dream for today, if one exists.
 
-------------------------------------------------------------------------------

Here's how this works:

1. I found a pdf of Einstein's Dreams, and used the open source utility
   `pdftotext` to convert that into a text file.

2. I wrote a Perl program, `dream_eater.pl` to gobble up each individual dream
   in the book and save it into a database. Perl's pretty good at text
   processing, so I chose to use it to hack together an ugly script to get the
   job done, instead of spending forever on step one trying to parse text with
   Haskell. Maybe some day I'll come back and clean it up and make it nice and
   Haskelly...

3. A small sqlite database, `dream_diary.db` holds a record of each dream and 
   the day that it occurs on.

4. `dreamer.hs` is the meat of it all. It is the Haskell program that uses Yesod
   to power the web site to display the dream, and will dynamically display a
   different dream depending on the time that it receives a request. The dream
   is fetched from the database using the Persistent library, for type-safe
   database interactions that mesh nicely with the rest of the Haskell code. The
   dream is then caught in the `dream_catcher.hamlet` HTML template, and served
   to the user. (Hamlet is a Haskell HTML template language for Haskell variable
   interpolation and other things.) Finally, this also powers a form on the
   website that a user can input their email into. This email will then be
   stored in the database to be used to send email alerts when a new dream is
   posted.

5. Finally, `pipe_dream.hs` is a smaller Haskell program to read the email
   addresses from the database, and send them emails through my gmail account
   with a Haskell gmail smtp library. 

```
                  +--------+
                  | ed.pdf |
                  +--------+
                      ||
                      ||
                  +--------+
                  | ed.txt |
                  +--------+
                      ||
                      ||
              +----------------+
              | dream_eater.pl |
              +----------------+
                      ||
                      ||
              +----------------+
        +-----| dream_diary.db |--+
        |     +----------------+  |
        |             ||          |
        |             ||          |
        |       +------------+    |
        |       | dreamer.hs |----+
        |       +------------+
        |             ||
        |             ||
        |  +----------------------+
        |  | dream_catcher.hamlet |---+
        |  +----------------------+   |
        |                             |  To you ->
        |                             +---------->
        |     +----------------+      |
        +-----| pipe_dreams.hs |------+
              +----------------+
```
