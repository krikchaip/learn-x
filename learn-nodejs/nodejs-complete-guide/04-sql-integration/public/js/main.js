const backdrop = document.querySelector('.backdrop')
const sideDrawer = document.querySelector('.mobile-nav')
const menuToggle = document.querySelector('#side-menu-toggle')

backdrop.addEventListener('click', function () {
  backdrop.style.display = 'none'
  sideDrawer.classList.remove('open')
})

menuToggle.addEventListener('click', function () {
  backdrop.style.display = 'block'
  sideDrawer.classList.add('open')
})
