from fabric.api import sudo, run, env, cd
from config import *

env.project_name = default_project_name


def update():
    with cd(env.directory):
        run('git pull')


def restart():
    sudo('supervisorctl restart %s' % env.project_name, shell=False)


def deploy():
    update()
    restart()


def tail_access():
    run('tail -f /var/log/nginx/%s_access.log' % env.project_name)
