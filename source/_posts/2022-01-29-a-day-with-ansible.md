---
title: A day with Ansible

---

While my work has been focused on frontend, I had to interact with backend systems on a few projects where I was either the most knowledgeable person on the team or working alone. In these cases, my usual solution was either serverless (Firebase Functions, AWS Lambda) or write an API using Node with Express and Laravel and hosting it with Heroku. I like the container paradigm of Heroku, starting fresh at every deployment and everything happens automatically upon pushing the changes.

But costs with Heroku ramp up quickly when scaling. I'm working on a side project and I decided to explore less expensive options, settling on a VPS at the end. I then quickly set it up one and my project was online.

I found that I'm not comfortable with manual operations,  when there is a non-zero chance that I will have to redo them in the future. Which, I assume, will be in a more stressful situation. I looked into automation tools and I found [Chef](https://www.chef.io/), [Ansible](https://www.ansible.com/), and [Puppet](https://puppet.com/) among others. I settled on Ansible due to being less complex to get started with.

# So, Ansible?

Installing Ansible was straightforward on both my M1 MBA and my Linux box. I needed something to test it with, so I used my Linux server as my learning environment because Vagrant (with VirtualBox) does not support M1 processors. A quick *Vagrantfile* later, and my virtual machine was ready.

As I learned more quickly by doing, I cloned a project that was doing the same thing I was aiming for, creating a deployment environment for Laravel. The project was last updated in 2018, so I figured there will be ample stuff to update in it.

The first thing I learned was “Inventory”. Inventory files lists all the hosts that you need to interact with, alongside their connection parameters. You can assign a name to them and group them. When using that inventory files, you can filter the particular hosts you want by using a pattern. When using Ansible to provision the virtual machine(s) created by vagrant, the latter will generate an inventory files that will automatically be used when provisioning.

The second thing I learned was “Roles”. Roles are collections of tasks and other related things (Variables, Templates, …) inside a common directory, which can later be shared if needed. Tasks are action items that will be executed against the hosts. I had one single host that has everything, but if you wanted a more modular architecture, you could have roles for the web servers, the database servers, the storage servers, including a shared role that contains the common tasks.

The third thing I learned was “Playbooks”. Playbooks are instruction manuals describing policies or executing a particular set of tasks. Inside the project I was using as a reference, there were playbooks for each particular server type, which included the `common` role and the role for that particular server and targeted a specific set of hosts inside the inventory. There was another playbook that included the others that could be used to provision a whole site.

# In the end

I've written a few bash and python scripts before. But, I was amazed by the possibilities that Ansible offered. My first use case was writing a playbook that will update my web application using git and install the new dependencies using composer when needed (which I used a lot as I'm actively developing it). There are numerous modules built by the community that will make the process a breeze (the above playbook only has two tasks). The documentation aspect alone is worth it when in a team context.
