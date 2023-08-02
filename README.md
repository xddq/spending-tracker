# Spending Tracker

Simple full stack web app for tracking purchases/expenses.

## Demo

A live demo of the app is hosted [here](https://spending-demo.pierre-dev.com).
It is "secured" with the basic auth credentials "demo" "demo". The data entered
there is reset on a daily basis. The demo might be taken offline at any point in
the future without updating the readme. The main page looks like this:

<img src="https://github.com/xddq/spending-tracker/blob/main/landing-page.png">

## Learnings

Some learnings I got by coding:

- [blaze-html](https://hackage.haskell.org/package/blaze-html) is a decent
  library to create web pages. Especially blaze-from-html is awesome. If you are
  new to blaze-html, you can html and then generate the required code for your
  views. Running something like `cat yourPage.html | blaze-from-html | xclip -sel c`
  I left some pages in ./html folder for demo purposes. This is how I approached creating pages initially.
- The "soft deletion" pattern with one table containing the deleted items
  (instead of having deleted_at in every damn table), which I have read about
  [here](https://www.brandur.org/fragments/deleted-record-insert) and
  [here](https://www.brandur.org/soft-deletion) seems to be a good pattern I might
  want to try using in other apps.
- [dotenv](https://hackage.haskell.org/package/dotenv) can be used to easily use
  environment variables.
- [docker-haskell](https://github.com/haskell/docker-haskell) hosts a number of
  docker images for haskell with ghc, cabal and stack ready to go. If you need a
  version combination of these tools that is not available, one can simply adapt
  the versions and shas and create an own image. That's what I did with the image
  that is available
  [here](https://hub.docker.com/repository/docker/xddqxddq/haskell/general).
- [bulma](https://bulma.io/) is an easy to use css-only library with decent
  documentation.
- GitLab has [schedules pipelines](https://docs.gitlab.com/ee/ci/pipelines/schedules.html)
  which basically allow for cron-like pipeline triggers. They are a decent fit
  for the auto-resetting demo app.

# Prerequisites

- Ensure you have ghc and cabal installed. I used
  [ghcup](https://www.haskell.org/ghcup/) to get these and used ghc 9.2.7 and
  cabal version 3.6.2.0.
- dbmate installed for raw SQL migrations
  - [install docs](https://github.com/amacneil/dbmate#installation)
- docker and docker-compose installed
- libpq-dev installed (for ubuntu 20.04 run `apt install libpq-dev`) I think
  this was required to be able to build postgresql-simple..?

## Quickstart

- Set up environment variables `cp env.local .env`
- Start postgresql/database `docker-compose up -d`
- Create the database `dbmate create`
- Create the tables for our app `dbmate up`
- Install dependencies and build the app `cabal build`
- Run the app `cabal run todo-app`
- Browse http://localhost:3000 to use the app
- For development it might be useful to run `bash watch-and-rebuild.sh` if you
  have inotify-watch installed it will then automatically rebuild whenever you
  make and save changes to the app.

## Managing the database

- [dbmate](https://github.com/amacneil/dbmate) is used for migrations, check
  their docu there if in doubt.
- Run dbmate to create migrations. Use snake_case since this is the default for
  postgres and dbmate. E.g. `dbmate new add_origin_created_at_to_recipes`
- A file will created which looks like this

```

-- migrate:up


-- migrate:down

```

## Hosting the app

I would suggest to host this app behind basic auth with secure credentials.
For the deployment, my automated deployment setup for GitLab can be found under
./.gitlab-ci.yml. I simply have a nginx reverse proxy in front of the app
secured with basic auth.

## Improvements

Happy about suggestions and improvements to the small code base. If you find
something confusing about "todo-app" in this code it is because I started with
the previous small project
[haskell-simple-todo](https://github.com/xddq/haskell-simple-todo) as baseline
and did not bother to adapt it in all places (e.g. databse name, cabal name,
etc..)
