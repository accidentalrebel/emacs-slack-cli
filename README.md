# Emacs Slack-Cli #

This is a simple Emacs interface for the [slack-cli python program](https://pypi.org/project/slack-cli/).

## Overview ##

There is an already existing slack client for Emacs called [emacs-slack](https://github.com/yuya373/emacs-slack). I've used it a lot in the past but could not get it to work anymore thanks to the changes to slack's API.

I searched around for an alternative and found the slack-cli python program. It had the bare minimum that I needed for my daily use so I installed it and made this interface for it.

## Installation ##
This package is not yet ready for Melpa so it can only be downloaded as is and loaded manually from your emacs config file like this:

`(load "~/your/path/slack-cli/slack-cli.el")`

Also make sure to install the `slack-cli` on your system. Found out how [here](https://pypi.org/project/slack-cli/). Run it once so you could get an API token.

## Configuration ##
From your configuration file, update and evaluate the line below:

`(setq slack-cli-channels '("channel1" "channel2" "channel3"))`

## Usage ##
Here are the available functions:

`slack-cli-listen` - Connects to a channel and waits for messages

`slack-cli-send` - Allows sending of a message to a specific channel

`slack-cli-retrieve` - Retrieves a certain amount of messages from a specific channel

## Contributions ##

This project has a lot of missing features and I will try to improve it whenever I can. Contributions are welcome. Refer to the todos or check out the issues list if there are any.

## To Do ##

  * Add checker if `slack-cli-channels` is set beforehand
  * Ability to reply to the channel from the channel's buffer
  * Upload to Melpa
