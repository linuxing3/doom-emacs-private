;;; private/defaults/config.el -*- lexical-binding: t; -*-


(if (featurep! +default-binding) (load! "+default-binding"))

(if (featurep! +extra-binding) (load! "+extra-binding"))