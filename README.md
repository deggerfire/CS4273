# CS4273

#Git Info

**If anything is unclear or you do not know how to use git ask your git master, we would rather you asked then blow up the git**

## Branch Structure

Main - The latest version of fully merged and “working” software. Assuming both groups are working in the same git only the git master should merge into this branch) **!You should NOT be working on this branch!**

Group-[x] - (undecided if we are doing this yet) The latest version of fully merged software for either group. **!You should NOT be working on this branch!**

[name] - Your branch that you should be working on. You can call it whatever you want so long as your name is on it (something like Nathan or Nathan-2)

## Working Conduct

It is important that you follow these rules because this will prevent merge conflicts which can take hours to sort out, trust me I’ve dealt with it before, not fun.

1) State in discord where/what you are working on to the relevant people. Also say when you are done working.

2) Don’t work on methods that someone says they are working on. If you need to mess with a method someone is working on, talk to them.

3) Keep up with the Group-x branch, before you start making changes to code, merge your branch to this one and when you are done merge Group-x to your branch. Working in this context refers to actively, if you are not actively working then your stuff should be on Group-x (unless the code does not work).
4) Do not make random changes around the code, only work where you have told people you are working, if you see something bad in the code just leave it and let people know in discord, at some point the chance will open up to fix it.

5) **COMMENT YOUR FREAKING CODE!** Add as many as possible and try to make your code look neat, everyone will thank you for it. If you are having a hard time adding comments, ask someone for help.

6) When you are trying to merge any branches that are not your own you should make sure all relevant people are informed.

7) Careful what you add to tracking, we don’t want something like add _pycache_. This is mainly an issue if you use git in vscode, I think _pycache_ is in gitinore but not 100% sure but that only covers it not other random compiler stuff.

These rules are not hard set; you can play fast and loose with them they are just general things to prevent git conflicts but if you do go breaking them understand that this could cause a lot of issues if you mess up and could cost people many hours, so be 100% sure you know what you are doing when you break them.


