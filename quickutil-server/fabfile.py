from fabric.api import sudo, run, env, cd
from config import *

env.project_name = default_project_name


def update():
    with cd(env.directory):
        run('git pull')


def start():
    sudo('supervisorctl start %s' % env.project_name, shell=False)


def stop():
    run('make -f /srv/www/quickutil/quickutil-server/Makefile SWANK_PORT=4095 stop')
    sudo('supervisorctl stop %s' % env.project_name, shell=False)


def restart():
    stop()
    start()


def deploy():
    update()
    restart()


def tail_access():
    run('tail -f /var/log/nginx/%s_access.log' % env.project_name)
