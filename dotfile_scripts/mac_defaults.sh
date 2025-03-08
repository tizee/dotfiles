#!/usr/bin/env bash
# configure macOS system via defaults command

if [[ $(uname -s) = "Darwin" ]]; then
  # backup macOS defaults before changing
  now=$(date -u "+%Y-%m-%d-%H%M%s")
  defaults read > "$HOME/Documents/defaults-$now.txt"

  osascript -e 'tell application "System Preferences" to quit'
  # display ~/Library
  chflags nohidden "$HOME/Library"

  if csrutil status | grep 'disabled' > /dev/null; then
    # Disable the sound effects on boot
    sudo nvram SystemAudioVolume=" "
    sudo nvram StartupMute=%01
    # Disable swap memory
    #VM_PAGER_COMPRESSOR_NO_SWAP 0x2 /* Active in-core compressor only. */
    #VM_PAGER_COMPRESSOR_WITH_SWAP 0x4 /* Active in-core compressor + swap backend. */
    sudo nvram boot-args="vm_compressor=2"
  fi

  # Allow apps download from anywhere to be opened
  # sudo spctl - master-disable

  # Miscellaneous {{{
  # Jump to the spot that's clicked on the scroll bar
  defaults write -g "AppleScrollerPagingBehavior" -bool true

  # Prefer tabs when opening documents
  defaults write -g "AppleWindowTabbingMode" -string "always"
  # Disable smart dash/period/quote substitutions
  defaults write -g "NSAutomaticDashSubstitutionEnabled" -bool false
  defaults write -g "NSAutomaticPeriodSubstitutionEnabled" -bool false
  defaults write -g "NSAutomaticQuoteSubstitutionEnabled" -bool false
  # Disable automatic capitalization
  defaults write -g "NSAutomaticCapitalizationEnabled" -bool false
  # speedup resizing for Cocoa applications
  defaults write -g "NSWindowResizeTime" -float 0.001
  # }}}

  # iCloud {{{
  # Save to disk (not to iCloud) by default
  defaults write -g "NSDocumentSaveNewDocumentsToCloud" -bool true
  # }}}

  # Time and Timezone {{{
  # do not use analog clock
  defaults write com.apple.menuextra.clock "IsAnalog" -bool false
  # Week Month day hour(12-hour format):minutes(leading-zero) AM/PM indicator
  defaults write com.apple.menuextra.clock "DateFormat" -string "EEE MMM d h:mm a"

  # Set the timezone; see `sudo systemsetup -listtimezones` for other values
  # sudo systemsetup -settimezone "America/New_York" >/dev/null
  # }}}

  # Keyboard {{{
  # disable press-and-hold for displaying accents menu
  defaults write -g "ApplePressAndHoldEnabled" -bool false
  # https://gist.github.com/hofmannsven/ff21749b0e6afc50da458bebbd9989c5
  # key repeat rate
  defaults write -g "InitialKeyRepeat" -int 10 # normal minimal is 15 (225ms)
  defaults write -g "KeyRepeat" -int 1 # normal minimal is 2 (30ms)
  # use keyboard navigation to move focus between controls (tab navigation)
  # possible values:
  # 0 - disable keyboard access for all controls(cannot interact with UI elements)
  # 1 - enable keyboard acess for all controls
  # 2 - use Tab to navigate between UI elements
  # 3 - enable additional keyboard navigation features, including navigate dialog boxex and pop-up menus
  defaults write -g "AppleKeyboardUIMode" -int 3

  # }}}

  # Dock {{{
  # killall Dock to activate
  # Dock position
  # possible values: left, bottom, right
  defaults write com.apple.dock "orientation" -string "right"
  # disable launch animation for saving a tad computing resources from eye candy
  defaults write com.apple.dock "launchanim" -bool false
  # enable autohide
  defaults write com.apple.dock "autohide" -bool true
  # show dock in seconds
  # It would be weird if set it to 0
  defaults write com.apple.dock "autohide-time-modifier" -float 0.1
  # autohide delay time in seconds
  defaults write com.apple.dock "autohide-delay" -float 0
  # minimize windows into their application’s icon
  defaults write com.apple.dock "minimize-to-application" -bool true
  # enable spring loading for all Dock items
  defaults write com.apple.dock "enable-spring-load-actions-on-all-items" -bool true
  # show indicator lights for open applications in the Dock
  defaults write com.apple.dock "show-process-indicators" -bool true
  # make Dock icons of hidden applications translucent
  defaults write com.apple.dock "showhidden" -bool true
  # don’t show recent applications in Dock
  defaults write com.apple.dock "show-recents" -bool false
  # don’t automatically rearrange Spaces based on most recent use
  defaults write com.apple.dock "mru-spaces" -bool false
  # speed up Mission Control animations
  defaults write com.apple.dock "expose-animation-duration" -float 0.1
  # group windows by application in Mission Control
  defaults write com.apple.dock "expose-group-by-app" -bool true

  # }}}

  # Menu Bar {{{
  # outdated
  # defaults write -g _HIHideMenuBar -bool false
  # }}}

  # Finder {{{
  # killall Finder to activate
  # display quit option
  defaults write com.apple.finder "QuitMenuItem" -bool "true"
  # always display file path at the bottom of Finder window
  defaults write com.apple.finder "ShowPathbar" -bool true
  # always show hidden files (⌘ Command + ⇧ Shift + . to toggle)
  defaults write com.apple.finder "AppleShowAllFiles" -bool true
  # use list view style by default
  # Possible values:
  # Nlsv - list view
  # clmv - column view
  # glyv - Gallery view
  # icnv - Icon view
  defaults write com.apple.finder "FXPreferredViewStyle" -string "Nlsv"
  # Keep folders on top when sorted by name
  defaults write com.apple.finder "_FXSortFoldersFirst" -bool true
  # Search scope
  # Possible values:
  # Sccf - search current folder
  # SCsp - use previous search scope
  # SCev - search this Mac
  defaults write com.apple.finder "FXDefaultSearchScope" -string "SCcf"
  # disable the warning when changing a file extension
  defaults write com.apple.finder "FXEnableExtensionChangeWarning" -bool false
  # show all filename extensions
  defaults write NSGlobalDomain AppleShowAllExtensions -bool true
  # show status bar
  defaults write com.apple.finder ShowStatusBar -bool true
  # show path bar
  defaults write com.apple.finder ShowPathbar -bool true
  # Display full POSIX path as Finder window title
  defaults write com.apple.finder _FXShowPosixPathInTitle -bool true
  # Keep folders on top when sorting by name
  defaults write com.apple.finder _FXSortFoldersFirst -bool true
  # }}}

  # Trackpad {{{
  # enable trackpad tap to click
  defaults write com.apple.AppleMultitouchTrackpad "Clicking" -bool true
  defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad "Clicking" -bool true

  # enable 3-finger drag
  defaults write com.apple.AppleMultitouchTrackpad "TrackpadThreeFingerDrag" -bool true
  defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad "TrackpadThreeFingerDrag" -bool true
  # }}}

  # Crash report {{{
  # Disable crash reporter
  defaults write com.apple.CrashReporter DialogType -string "none"
  # }}}

  # Screensaver {{{
  # require password when screensaver begins
  defaults write com.apple.screensaver "askForPassword" -int 1
  defaults write com.apple.screensaver "askForPasswordDelay" -int 0

  # }}}

  # Print {{{
  # always display expanded options when print
  defaults write NSGlobalDomain "PMPrintingExpandedStateForPrint" -bool true
  # always display expanded options when save
  defaults write NSGlobalDomain "NSNavPanelExpandedStateForSaveMode" -bool true
  # Exit on finish
  defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true
  # }}}

  # Screenshot {{{
  # killall SystemUIServer to activate
  # do not include date and time in screenshot files
  defaults write com.apple.screencapture "include-date" -bool false
  # disable shadow effect
  defaults write com.apple.screencapture "disable-shadow" -bool true
  # save location
  defaults write com.apple.screencapture "location" -string "$HOME/Pictures/Screenshots"
  # simulator screenshot location
  defaults write com.apple.iphonesimulator "ScreenShotSaveLocation" -string "$HOME/Pictures/Screenshots"
  # }}}

  # Feedback assistant {{{
  # do not gather large files when submitting a report
  defaults write com.apple.appleseed.FeedbackAssistant "Autogather" -bool false
  # }}}

  # XCode {{{
  # show build duration in XCode's toolbar
  defaults write com.apple.dt.Xcode "ShowBuildOperationDuration" -bool true
  # }}}

  # Email {{{
  # Copy addresses as `foo@example.com` instead of `Foo Bar <foo@example.com>`
  defaults write com.apple.mail "AddressesIncludeNameOnPasteboard" -bool false
  # Disable send and reply animations in Mail.app
  defaults write com.apple.mail "DisableReplyAnimations" -bool true
  defaults write com.apple.mail "DisableSendAnimations" -bool true
  # Most recent first
  defaults write com.apple.mail "ConversationViewSortDescending" -bool true
  # }}}

  # Spotlight {{{
  # Set spotlight indexing order
  defaults write com.apple.spotlight orderedItems -array \
  '{"enabled" = 1;"name" = "APPLICATIONS";}' \
  '{"enabled" = 1;"name" = "MENU_CONVERSION";}' \
  '{"enabled" = 1;"name" = "MENU_DEFINITION";}' \
  '{"enabled" = 1;"name" = "SYSTEM_PREFS";}' \
  '{"enabled" = 1;"name" = "DIRECTORIES";}' \
  '{"enabled" = 1;"name" = "DOCUMENTS";}' \
  '{"enabled" = 1;"name" = "PDF";}' \
  '{"enabled" = 0;"name" = "FONTS";}' \
  '{"enabled" = 0;"name" = "MESSAGES";}' \
  '{"enabled" = 0;"name" = "CONTACT";}' \
  '{"enabled" = 0;"name" = "EVENT_TODO";}' \
  '{"enabled" = 0;"name" = "IMAGES";}' \
  '{"enabled" = 0;"name" = "BOOKMARKS";}' \
  '{"enabled" = 0;"name" = "MUSIC";}' \
  '{"enabled" = 0;"name" = "MOVIES";}' \
  '{"enabled" = 0;"name" = "PRESENTATIONS";}' \
  '{"enabled" = 0;"name" = "SPREADSHEETS";}' \
  '{"enabled" = 0;"name" = "SOURCE";}' \
  '{"enabled" = 0;"name" = "MENU_OTHER";}' \
  '{"enabled" = 0;"name" = "MENU_EXPRESSION";}' \
  '{"enabled" = 0;"name" = "MENU_WEBSEARCH";}' \
  '{"enabled" = 0;"name" = "MENU_SPOTLIGHT_SUGGESTIONS";}'
  # }}}

  # Hot corners {{{
  # Possible values:
  #  0: no-op
  #  2: Mission Control
  #  3: Show application windows
  #  4: Desktop
  #  5: Start screen saver
  #  6: Disable screen saver
  #  7: Dashboard
  # 10: Put display to sleep
  # 11: Launchpad
  # 12: Notification Center
  # 13: Lock screen
  # Top right screen corner → Desktop
  defaults write com.apple.dock wvous-tr-corner -int 4
  defaults write com.apple.dock wvous-tr-modifier -int 0
  # Bottom right screen corner → Mission Control
  defaults write com.apple.dock wvous-br-corner -int 2
  defaults write com.apple.dock wvous-br-modifier -int 0
  # Bottom left screen corner
  defaults write com.apple.dock wvous-bl-corner -int 0
  defaults write com.apple.dock wvous-bl-modifier -int 0
  # }}}

  # Activity Monitory {{{
  # Sort by CPU usage
  defaults write com.apple.ActivityMonitor "SortColumn" -string "CPUUsage"
  defaults write com.apple.ActivityMonitor "SortDirection" -int 0
  # }}}

  # Launch Services {{{
  # Disable quarantine for downloaded apps
  defaults write com.apple.LaunchServices LSQuarantine -bool false
  # }}}

  # Deskstop Services {{{
  # avoid creating .DS_Store files on network or USB volumes
  defaults write com.apple.desktopservices "DSDontWriteNetworkStores" -bool true
  defaults write com.apple.desktopservices "DSDontWriteUSBStores" -bool true
  # }}}

  # Safari {{{
  # For better privacy
  defaults write com.apple.Safari "UniversalSearchEnabled" -bool false
  defaults write com.apple.Safari "SuppressSearchSuggestions" -bool true
  defaults write com.apple.Safari "SendDoNotTrackHTTPHeader" -bool true

  # Disable auto open downloads
  defaults write com.apple.Safari "AutoOpenSafeDownloads" -bool false

  # Enable Develop Menu, Web Inspector
  defaults write com.apple.Safari "IncludeDevelopMenu" -bool true
  defaults write com.apple.Safari "IncludeInternalDebugMenu" -bool true
  defaults write com.apple.Safari "WebKitDeveloperExtras" -bool true
  defaults write com.apple.Safari "WebKitDeveloperExtrasEnabledPreferenceKey" -bool true
  defaults write com.apple.Safari "com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled" -bool true
  # }}}

  # Disable disk image verification {{{
  defaults write com.apple.frameworks.diskimages "skip-verify" -bool true
  defaults write com.apple.frameworks.diskimages "skip-verify-locked" -bool true
  defaults write com.apple.frameworks.diskimages "skip-verify-remote" -bool true
  # }}}

  # Apple AdLib {{{
  # Disable personlized advertising
  defaults com.apple.AdLib "forceLimitAdTracking" -bool true
  defaults com.apple.AdLib "allowApplePersonalizedAdvertising" -bool false
  defaults com.apple.AdLib "allowIdentifierForAdvertising" -bool false
  # }}}

  # enable debug menu from AppKit for all applications
  # see .config/bin/num2str
  # 0x4445425547 in utf-8 encoding -> DEBUG
  defaults write -g _NS_4445425547 -bool true

  # enforce reloading system setttings
  # https://apple.stackexchange.com/questions/405937/how-can-i-enable-keyboard-shortcut-preference-after-modifying-it-through-defaul/414836#414836
  /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
fi
# vim:foldmethod=marker
