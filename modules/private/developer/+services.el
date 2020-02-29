;;; private/writer/+services

(defun my-prodigy-services ()
  "Prodigy is the service manager"
    (progn
        (prodigy-define-service
          :name "Gatsby Netlify: International Study"
          :command "yarn"
          :args '("start")
          :cwd "~/workspace/international-policy-study"
          :tags '(dev)
          :stop-signal 'sigkill
          :kill-process-buffer-on-stop t)

        (prodigy-define-service
          :name "Gridsome Netlify: Gridsome.org"
          :command "yarn"
          :args '("develop")
          :cwd "~/workspace/gridsome"
          :tags '(dev)
          :stop-signal 'sigkill
          :kill-process-buffer-on-stop t)

        (prodigy-define-service
          :name "Docker Machine default"
          :command "docker-machine"
          :args '("start" "default")
          :cwd "~/workspace"
          :tags '(dev)
          :stop-signal 'sigkill
          :kill-process-buffer-on-stop t)

        (prodigy-define-service
          :name "Prisma Local Server: 4466"
          :command "docker-compose"
          :cwd "~/workspace/gridsome"
          :tags '(dev)
          :stop-signal 'sigkill
          :kill-process-buffer-on-stop t)

        (prodigy-define-service
          :name "Information Center: El Universal"
          :command "scrapy"
          :args '("crawl" "eluniversal")
          :cwd "~/Dropbox/shared/InformationCenter"
          :tags '(work)
          :stop-signal 'sigkill
          :kill-process-buffer-on-stop t)

        (prodigy-define-service
          :name "----------华丽的分割线---------------")

        (prodigy-define-service
          :name "Nikola Blog Local Serve"
          :command "nikola"
          :args '("serve" "-p" "9999")
          :cwd "~/Dropbox/xingwenju.com/nikola"
          :tags '(work)
          :stop-signal 'sigkill
          :kill-process-buffer-on-stop t)

        (prodigy-define-service
          :name "Nikola Blog Build"
          :command "nikola"
          :args '("build")
          :cwd "~/Dropbox/xingwenju.com/nikola"
          :tags '(work)
          :stop-signal 'sigkill
          :kill-process-buffer-on-stop t)

        (prodigy-define-service
          :name "Nikola Blog Deploy Github"
          :command "nikola"
          :args '("github_deploy")
          :cwd "~/Dropbox/xingwenju.com/nikola"
          :tags '(work)
          :stop-signal 'sigkill
          :kill-process-buffer-on-stop t)

        (prodigy-define-service
          :name "----------华丽的分割线---------------"
          :command "")

        (prodigy-define-service
          :name "Run Dev Server of awesome manager"
          :command "yarn"
          :args '("dev")
          :cwd "~/workspace/awesome-manager"
          :tags '(work)
          :stop-signal 'sigkill
          :kill-process-buffer-on-stop t)

        (prodigy-define-service
          :name "Run Jest of awesome manager"
          :command "yarn"
          :args '("test:unit:watch")
          :cwd "~/workspace/awesome-manager"
          :tags '(work)
          :stop-signal 'sigkill
          :kill-process-buffer-on-stop t)

        (prodigy-define-service
          :name "Run Storybook of awesome manager"
          :command "yarn"
          :args '("serve:storybook")
          :cwd "~/workspace/awesome-manager"
          :tags '(work)
          :stop-signal 'sigkill
          :kill-process-buffer-on-stop t)

        (prodigy-define-service
          :name "----------华丽的分割线---------------")

        (prodigy-define-service
          :name "Run hyde Hugo Site Server"
          :command "hugo"
          :args '("server" "--theme=after-dark" "--buildDrafts")
          :cwd "~/Dropbox/xingwenju.com/hugo"
          :tags '(work)
          :stop-signal 'sigkill
          :kill-process-buffer-on-stop t)

        (prodigy-define-service
          :name "Run after-dark Hugo Site Server"
          :command "hugo"
          :args '("server" "--theme=after-dark" "--buildDrafts")
          :cwd "~/Dropbox/xingwenju.com/hugo"
          :tags '(work)
          :stop-signal 'sigkill
          :kill-process-buffer-on-stop t)

        (prodigy-define-service
          :name "----------华丽的分割线---------------")

    ))

(my-prodigy-services)
