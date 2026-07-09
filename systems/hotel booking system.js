import React, { useState } from 'react';
import { 
  Home, LogIn, LayoutDashboard, Bed, Users, Calendar, 
  ArrowLeftRight, CreditCard, BarChart3, UserCheck, 
  ChevronDown, ChevronRight, Menu, X 
} from 'lucide-react';

export default function SidebarNavigation() {
  const [isOpen, setIsOpen] = useState(true);
  const [expandedMenus, setExpandedMenus] = useState({});

  // Toggle submenus
  const toggleSubmenu = (title) => {
    setExpandedMenus(prev => ({
      ...prev,
      [title]: !prev[title]
    }));
  };

  // Navigation Data Structure mapping exactly to your sitemap
  const navItems = [
    { title: 'Login', icon: LogIn, path: '/login' },
    { title: 'Dashboard', icon: LayoutDashboard, path: '/dashboard' },
    {
      title: 'Rooms',
      icon: Bed,
      submenu: [
        { title: 'View Rooms', path: '/rooms/view' },
        { title: 'Add Room', path: '/rooms/add' },
        { title: 'Edit Room', path: '/rooms/edit' },
        { title: 'Delete Room', path: '/rooms/delete' },
      ]
    },
    {
      title: 'Guests',
      icon: Users,
      submenu: [
        { title: 'Register Guest', path: '/guests/register' },
        { title: 'Guest List', path: '/guests/list' },
        { title: 'Guest Details', path: '/guests/details' },
      ]
    },
    {
      title: 'Reservations',
      icon: Calendar,
      submenu: [
        { title: 'New Reservation', path: '/reservations/new' },
        { title: 'Reservation List', path: '/reservations/list' },
        { title: 'Edit Reservation', path: '/reservations/edit' },
      ]
    },
    { title: 'Check In / Check Out', icon: ArrowLeftRight, path: '/check-in-out' },
    {
      title: 'Payments',
      icon: CreditCard,
      submenu: [
        { title: 'Create Invoice', path: '/payments/invoice' },
        { title: 'Payment History', path: '/payments/history' },
        { title: 'Receipts', path: '/payments/receipts' },
      ]
    },
    {
      title: 'Reports',
      icon: BarChart3,
      submenu: [
        { title: 'Occupancy Report', path: '/reports/occupancy' },
        { title: 'Revenue Report', path: '/reports/revenue' },
        { title: 'Booking Report', path: '/reports/booking' },
      ]
    },
    {
      title: 'User Management',
      icon: UserCheck,
      submenu: [
        { title: 'Staff Accounts', path: '/users/staff' },
        { title: 'Roles & Permissions', path: '/users/roles' },
      ]
    }
  ];

  return (
    <div className="flex h-screen bg-gray-100">
      {/* Sidebar */}
      <aside className={`bg-gray-900 text-gray-100 transition-all duration-300 flex flex-col ${isOpen ? 'w-64' : 'w-20'}`}>
        
        {/* Header / Brand */}
        <div className="p-4 flex items-center justify-between border-b border-gray-800">
          <div className={`flex items-center space-x-2 ${!isOpen && 'hidden'}`}>
            <Home className="h-6 w-6 text-indigo-400" />
            <span className="font-bold text-lg tracking-wide">Hotel PMS</span>
          </div>
          <button 
            onClick={() => setIsOpen(!isOpen)} 
            className="p-1.5 rounded-lg hover:bg-gray-800 transition-colors"
          >
            {isOpen ? <X size={20} /> : <Menu size={20} />}
          </button>
        </div>

        {/* Navigation List */}
        <nav className="flex-1 overflow-y-auto p-3 space-y-1 custom-scrollbar">
          {navItems.map((item, index) => {
            const Icon = item.icon;
            const hasSubmenu = !!item.submenu;
            const isExpanded = expandedMenus[item.title];

            return (
              <div key={index} className="space-y-1">
                {/* Main Link / Dropdown Trigger */}
                <button
                  onClick={() => hasSubmenu ? toggleSubmenu(item.title) : null}
                  className={`w-full flex items-center justify-between p-3 rounded-lg transition-colors hover:bg-gray-800 text-left
                    ${hasSubmenu && isExpanded ? 'bg-gray-800/50' : ''}`}
                >
                  <div className="flex items-center space-x-3">
                    <Icon className="h-5 w-5 text-gray-400 shrink-0" />
                    {isOpen && <span className="text-sm font-medium">{item.title}</span>}
                  </div>
                  
                  {isOpen && hasSubmenu && (
                    <span className="text-gray-500">
                      {isExpanded ? <ChevronDown size={16} /> : <ChevronRight size={16} />}
                    </span>
                  )}
                </button>

                {/* Submenu Items */}
                {isOpen && hasSubmenu && isExpanded && (
                  <div className="pl-11 pr-2 py-1 space-y-1 border-l border-gray-800 ml-5">
                    {item.submenu.map((subItem, subIndex) => (
                      <a
                        key={subIndex}
                        href={subItem.path}
                        className="block p-2 text-xs font-medium text-gray-400 hover:text-white rounded transition-colors hover:bg-gray-800"
                      >
                        {subItem.title}
                      </a>
                    ))}
                  </div>
                )}
              </div>
            );
          })}
        </nav>
      </aside>

      {/* Main Content Area Placeholder */}
      <main className="flex-1 p-8 overflow-y-auto">
        <h1 className="text-2xl font-bold text-gray-800">Main Content Canvas</h1>
        <p className="text-gray-600 mt-2">Select an option from the sidebar to navigate the system.</p>
      </main>
    </div>
  );
}