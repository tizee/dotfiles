#!/usr/bin/env zx
// list stashed file changes

$.cmd = "/usr/bin/zsh";
$.verbose = false;

try {
  let stash_output = await $`git stash list`;
  let list = stash_output.stdout
    .trim()
    .split("\n")
    .map((stash) => stash.match(/^(stash@{\d+})/)[0]);
  let index = await question("Select a stash to view (number): ", {
    choices: list.map((_, index) => index),
  });
  let stash = list[+index];
  (await $`git show --pretty="" --name-only ${stash}`).stdout
    .trim()
    .split("\n")
    .map((file) => file.trim())
    .map(async (file) => {
      try {
        await $`git show --pretty="" ${stash}:${file} `;
      } catch (err) {
        await $`echo ${err}`;
      }
    });
} catch (err) {}
