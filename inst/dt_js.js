(function() {
  'use strict';
  angular
    .module('MyApp', ['ngMaterial', 'dndLists'])
    .controller('AppCtrl', AppCtrl)
    .service('local', local)
    .config(function($mdThemingProvider) {
      $mdThemingProvider.theme('default')
        .primaryPalette('blue-grey')
        .accentPalette('pink')
        .warnPalette('orange');

    });

  function AppCtrl($scope, $log, $mdDialog, local) {
  var tabs = [
              { title: 'To do', content: "Enter ." }

        ],
            selected = null,
            previous = null;
        $scope.tabs = tabs;
        $scope.selectedIndex = 2;
        $scope.$watch('selectedIndex', function (current, old) {
            previous = selected;
            selected = tabs[current];
            if (old + 1 && (old != current)) $log.debug('Goodbye ' + previous.title + '!');
            if (current + 1) $log.debug('Hello ' + selected.title + '!');
        });

    $scope.lists = local.get(),
      $scope.selectedIndex = local.getTab();

    $scope.saveTab = function() {
      local.setTab($scope.selectedIndex);
    }

    $scope.addTab = function() {
      $scope.lists.push({
        title: $scope.tTitle,
        items: [],
        disabled: false
      });
      $scope.tTitle = "";
      local.set($scope.lists)
    };
    $scope.removeTab = function(tab) {
      if ($scope.lists.length === 1) {
        return
      }
      var index = $scope.lists.indexOf(tab);
      $scope.lists.splice(index, 1);
      local.set($scope.lists);
    };

    $scope.inputItem = [];
    $scope.addItem = function(listIndex) {
      if ($scope.inputItem[listIndex] == "") {
        return
      }
      $scope.lists[listIndex].items.push({
        name: $scope.inputItem[listIndex],
        completed: false
      });
      local.set($scope.lists);
      $scope.inputItem[listIndex] = '';
    }

    $scope.updateCompleted = function() {
      local.set($scope.lists);
    }

    $scope.clear = function(index) {
      for (var i = 0; i < $scope.lists[index].items.length; i++) {
        if ($scope.lists[index].items[i].completed) {
          $scope.lists[index].items[i] = null;
        }
      }
      $scope.lists[index].items = $scope.lists[index].items.filter(function(item) {
        return item !== null
      });
      local.set($scope.lists);
    }

    $scope.deleteItem = function(listIndex, itemIndex) {
      $scope.lists[listIndex].items.splice(itemIndex, 1);
      local.set($scope.lists);
    }

    $scope.update = function() {
      local.set($scope.lists);
    }

  }

   function local() {
        this.get = function () {
            var defaultList = [
              {
                  title: 'Todo',
                  disabled: false,
                  items: [
                    {
                        name: 'local-storage lists app',
                        amount: '500',
                        completed: false
                    },
                    {
                        name: 'Add a new list item above',
                      amount: '500',
                        completed: true,
                    },
                    {
                        name: 'Create a new list with the form below',
                        amount: '500',
                      completed: false
                    },
                    {
                        name: 'Drag list items to reorder',
                      amount: '500',
                        completed: false
                    }

                  ]
              },
            {
                title: 'Groceries',
                disabled: false,
                items: [
                  {
                      name: 'Multple tabs',
                      completed: false
                  },
                  {
                      name: 'How cool!',
                      completed: false,
                  },
                  {
                      name: 'I should buy fruit so my insides don\'t rot',
                      completed: true
                  }
                ]
            }
            ];

            if (localStorage !== null) {
                return localStorage.getItem('lists') !== null ? JSON.parse(localStorage.getItem('lists')) : defaultList;
            } else {
                return defaultList;
            }
        };
        this.set = function (lists) {
            if (localStorage !== null) {
                localStorage.setItem('lists', JSON.stringify(lists));
            }
        }
        this.getTab = function () {
            if (localStorage !== null) {
                return localStorage.getItem('savedTab') !== null ? JSON.parse(localStorage.getItem('savedTab')) : 0;
            } else {
                return 0;
            }
        }
        this.setTab = function (tab) {
            if (localStorage !== null) {
                localStorage.setItem('savedTab', JSON.stringify(tab));
            }
        }

    };



})();
