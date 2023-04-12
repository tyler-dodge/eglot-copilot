# eglot-copilot.el
[![License](https://img.shields.io/badge/license-GPL_3-green.svg)](https://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](https://melpa.org/packages/eglot-copilot-badge.svg)](https://melpa.org/#/org-runbook)
[![Version](https://img.shields.io/github/v/tag/tyler-dodge/eglot-copilot)](https://github.com/tyler-dodge/eglot-copilot/releases)

---

 Copilot integration with EGLOT and Company

## Usage

'eglot-copilot-setup' Handles integration with emacs. 
```
(eglot-copilot-setup)
```
'eglot-copilot-node-agent-script' will need to be updated to point to an copilot agent.js.
An example can be found at https://github.com/github/copilot.vim/blob/release/copilot/dist/agent.js

This package provides a company transformer named 'eglot-copilot-sort-results-company-transformer' 
which takes the results of the copilot panel, and sorts the company candidates based on their presence in
the copilot panel. 
It is added to the transformer list by default in 'eglot-copilot-setup'



## Commands

* [eglot-copilot-setup](#eglot-copilot-setup) <a name="eglot-copilot-setup"></a>
Setup the copilot integration to ensure the best user experience.

* [eglot-copilot-counsel](#eglot-copilot-counsel) <a name="eglot-copilot-counsel"></a>
Runs ivy to preview results from copilot.

* [eglot-copilot-sign-in](#eglot-copilot-sign-in) <a name="eglot-copilot-sign-in"></a>
Shows the sign-in form for authenticating with copilot.

* [eglot-copilot-check-status](#eglot-copilot-check-status) <a name="eglot-copilot-check-status"></a>
Checks the login status for copilot authentication.

* [eglot-copilot-company](#eglot-copilot-company) <a name="eglot-copilot-company"></a>
Company backend for completing using copilot suggestions.

* [eglot-copilot-panel-refresh](#eglot-copilot-panel-refresh) <a name="eglot-copilot-panel-refresh"></a>
Refresh the copilot suggestions buffer for the current buffer.

* [eglot-copilot-sort-results-company-transformer](#eglot-copilot-sort-results-company-transformer) <a name="eglot-copilot-sort-results-company-transformer"></a>
Company transformer for sorting results based on the output from copilot.
See ‘company-transformers’.


## Customization

* [eglot-copilot-node-program](#eglot-copilot-node-program)<a name="eglot-copilot-node-program"></a>
Path to node executable used to run the copilot agent.
As of this writing, Node version must be less than 18.

* [eglot-copilot-node-agent-script](#eglot-copilot-node-agent-script)<a name="eglot-copilot-node-agent-script"></a>
Script that runs the copilot agent.


## Contributing

Contributions welcome, but forking preferred. 
I plan to actively maintain this, but I will be prioritizing features that impact me first.

I'll look at most pull requests eventually, but there is no SLA on those being accepted. 
    
Also, I will only respond to pull requests on a case by case basis. 
I have no obligation to comment on, justify not accepting, or accept any given pull request. 
Feel free to start a fork that has more support in that area.

If there's a great pull request that I'm slow on accepting, feel free to fork and rename the project.
